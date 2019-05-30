package io.github.vlsergey.kremlin2wikisource;

import static io.github.vlsergey.kremlin2wikisource.TextProcessor.assertNotBlank;
import static io.github.vlsergey.kremlin2wikisource.TextProcessor.assertNotNull;

import io.github.vlsergey.kremlin2wikisource.TextProcessor.ToUpload;
import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
@Slf4j
@SuppressWarnings("deprecation")
public class Importer {

	private static final Scanner scan = new Scanner(System.in);

	static String prepareHtml(String html) {
		return html.replace('\u00a0', ' ');
	}

	private static Boolean prompt(String prmpt) {
		List<String> allowed = Arrays.asList(Boolean.FALSE.toString().toLowerCase(),
				Boolean.TRUE.toString().toLowerCase());
		String word = "";
		do {
			System.out.print(prmpt);
			if (scan.hasNextLine()) {
				word = scan.nextLine().toLowerCase().trim();
			}
		} while (!allowed.contains(word)); // no need for == true
		return Boolean.valueOf(word);
	}

	@Autowired
	private RestTemplate restTemplate;

	@Autowired
	private TextProcessor textProcessor;

	@Autowired
	private WikidataApiHelper wikidataApiHelper;

	@Autowired
	private WikisourceApiHelper wikisourceApiHelper;

	private void importFrom(URI originalUrl) throws Exception {

		int page = 1;
		URI nextUrl = originalUrl;

		String html, title = null, summary = null, content = null;
		do {
			html = restTemplate.getForEntity(nextUrl, String.class).getBody();
			html = prepareHtml(html);

			if (page == 1) {
				title = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html, "<h1>", "</h1>"));
				summary = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html,
						"<div class=\"read__lead entry-summary p-summary\" itemprop=\"description\">", "</div>"));
				content = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html, "<pre>", "</pre>"));
				if (content == null) {
					content = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html, "<pre>", "</div>"));
				}

				assertNotBlank(title);
				assertNotNull(summary);
				assertNotBlank(content);
			} else {
				content += "\n";
				String nextPageContent = StringUtils.substringBetween(html, "<pre class=\"masha-ignore\">", "</pre>");
				if (nextPageContent == null) {
					nextPageContent = StringEscapeUtils.unescapeHtml4(
							StringUtils.substringBetween(html, "<div class=\"reader_act_body\">", "</div>"));
				}

				assertNotBlank(nextPageContent);
				content += StringEscapeUtils.unescapeHtml4(nextPageContent);
			}
			nextUrl = URI.create(originalUrl + "/page/" + (++page));
		} while (html.contains("Показать следующую страницу документа"));
		content = StringEscapeUtils.unescapeHtml4(content);

		ToUpload toUpload;
		if (title.startsWith("Распоряжение Президента Российской Федерации от")) {
			toUpload = textProcessor.importAsРаспоряжение(originalUrl, title, summary, content);
		} else if (title.startsWith("Указ Президента Российской Федерации")) {
			toUpload = textProcessor.importAsУказ(originalUrl, title, summary, content);
		} else if (title.startsWith("Федеральный конституционный закон")) {
			return;
		} else if (title.startsWith("Федеральный закон")) {
			return;
		} else {
			throw new AssertionError("Unsupported: \"" + title + "\" from " + originalUrl);
		}

		System.out.println();
		System.out.println(toUpload.getWikisourceTitle());
		System.out.println(toUpload.getWikisourceContent());

		if (!prompt("Agree to upload? ")) {
			throw new RuntimeException("Aborted");
		}

		final String editSummary = "Importing from " + originalUrl;
		wikisourceApiHelper.edit(editSummary, toUpload.getWikisourceTitle(), toUpload.getWikisourceContent());
		wikidataApiHelper.wbeditentity_new(editSummary, toUpload.getWikidataData());
	}

	public void run() throws Exception {
		// first was 18272
		for (int pageId = 18653; pageId <= 20000; pageId++) {
			URI uri = URI.create("http://www.kremlin.ru/acts/bank/" + pageId);
			try {
				importFrom(uri);
			} catch (Throwable exc) {
				log.error("Unable to import from " + uri);
				throw exc;
			}
		}
	}
}
