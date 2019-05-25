package io.github.vlsergey.kremlin2wikisource;

import java.net.URI;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.Data;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
@SuppressWarnings("deprecation")
public class Importer {

    @Data
    private static class ToUpload {
        private String wikidataData;
        private String wikisourceContent;
        private String wikisourceTitle;
    }

    static String replaceAll(String src, String regexp, BiFunction<Matcher, String, String> replacement) {
        final Matcher matcher = Pattern.compile(regexp).matcher(src);
        matcher.reset();
        boolean result = matcher.find();
        if (result) {
            StringBuffer sb = new StringBuffer();
            do {
                String replace = replacement.apply(matcher, matcher.group(1));
                matcher.appendReplacement(sb, replace);
                result = matcher.find();
            } while (result);
            matcher.appendTail(sb);
            return sb.toString();
        }
        return src;
    }

    static String wikilinkHeaders(String src, String title, String wikilink) {
        String newTitle = title;
        newTitle = newTitle.replaceAll(" ([IVX]+) СТЕПЕНИ", " {{nobr|$1 СТЕПЕНИ}}");
        newTitle = newTitle.replaceAll(" ([IVX]+) ЛЕТ", " {{nobr|$1 ЛЕТ}}");

        return src.replaceAll(Pattern.quote("\n=== " + title + " ===\n"),
                "\n=== [[:w:ru:" + wikilink + "|" + newTitle + "]] ===\n");
    }

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private WikidataApiHelper wikidataApiHelper;

    @Autowired
    private WikisourceApiHelper wikisourceApiHelper;

    private void assertNotBlank(String str) {
        if (StringUtils.isBlank(str)) {
            throw new AssertionError();
        }
    }

    private ToUpload importAsУказ(URI url, String title, String summary, String contentWithHeader) throws Exception {
        contentWithHeader = contentWithHeader.replaceAll("\\r", "");

        final Pattern titlePattern = Pattern.compile("^Указ Президента Российской Федерации от (.*) г. № (\\d+)$");
        final Matcher titleMatcher = titlePattern.matcher(title);
        if (!titleMatcher.matches())
            throw new AssertionError();
        String docDateStr = titleMatcher.group(1);
        String docNumber = titleMatcher.group(2);

        SimpleDateFormat format = new SimpleDateFormat("dd.MM.yyyy");
        final Date docDate = format.parse(docDateStr);

        String content = StringUtils.substringAfter(contentWithHeader, "ПРЕЗИДЕНТА РОССИЙСКОЙ ФЕДЕРАЦИИ\n\n\n");
        assertNotBlank(content);
        content = StringUtils.substringAfter(content, "\n\n");
        assertNotBlank(content);

        content = "\n" + content.trim() + "\n";

        final Matcher signMatcher =
                Pattern.compile("\\n[ ]+Президент Российской Федерации\\s+([^ ].*)\\n").matcher(content);
        if (!signMatcher.find())
            throw new AssertionError();

        content = signMatcher.replaceFirst("\n{{Подпись|Президент Российской Федерации|$1}}\n");

        final int signStart = content.indexOf("{{Подпись|Президент Российской Федерации|");
        {
            final Matcher signPlaceMatcher = Pattern.compile("\\n[ ]+(Москва\\,[ ]*Кремль)\\n").matcher(content);
            if (!signPlaceMatcher.find(signStart))
                throw new AssertionError();
            content = signPlaceMatcher.replaceFirst("\n{{left|$1}}\n");
        }

        {
            final String expectedSignatureDate = new SimpleDateFormat("d MMMM yyyy").format(docDate) + " года";
            final Matcher signDateMatcher =
                    Pattern.compile("\\n[ ]+(" + Pattern.quote(expectedSignatureDate) + ")\\n").matcher(content);
            if (!signDateMatcher.find(signStart))
                throw new AssertionError();
            content = signDateMatcher.replaceFirst("\n{{left|$1}}\n");
        }

        {
            final Matcher signNumberMatcher = Pattern.compile("\\n[ ]+N " + docNumber + "\\n").matcher(content);
            if (!signNumberMatcher.find(signStart))
                throw new AssertionError();
            content = signNumberMatcher.replaceFirst("\n{{left|№ " + docNumber + "}}\n");
        }

        content = processTextContent(content);

        String wikiContent = MessageFormat.format("'{{'Указ Президента РФ\n" //
                + "| НОМЕР          = {0}\n" //
                + "| ДАТА           = {1}\n" //
                + "| НАЗВАНИЕ       = {2}\n" //
                + "| ДАТАПУБЛИКАЦИИ = \n" //
                + "| ИСТОЧНИК       = {3}\n" //
                + "| ДРУГОЕ         = \n" //
                + "| УТРАТИЛ СИЛУ   = \n" //
                + "| ПРЕДЫДУЩИЙ     = \n" //
                + "| СЛЕДУЮЩИЙ      = \n" //
                + "| КАЧЕСТВО       = \n" //
                + "'}}'<div class=\"text\">\n\n{4}\n\n</div>", docNumber, docDateStr, summary, url, content.trim());
        System.out.println();
        System.out.println(wikiContent);

        final String articleTitle = "Указ Президента РФ от " + docDateStr + " № " + docNumber;
        final int docTypeEntityId = 2061228;

        ToUpload toUpload = new ToUpload();
        toUpload.setWikisourceTitle(articleTitle);
        toUpload.setWikisourceContent(wikiContent);
        toUpload.setWikidataData("{\"labels\":{\"ru\":{\"language\":\"ru\",\"value\":\""
                + StringEscapeUtils.escapeJson(articleTitle) + "\"}},"
                + "\"descriptions\":{\"ru\":{\"language\":\"ru\",\"value\":\"" + StringEscapeUtils.escapeJson(summary)
                + "\"}}," + "\"claims\":["
                + ("{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P31\",\"hash\":\"jw26z0hw0lllqvutyommuenjothwwuzbdiirztir\",\"datavalue\":{\"value\":{\"entity-type\":\"item\",\"numeric-id\":\""
                        + docTypeEntityId + "\",\"id\":\"Q" + docTypeEntityId
                        + "\"},\"type\":\"wikibase-entityid\"},\"datatype\":\"wikibase-item\"},\"type\":\"statement\",\"rank\":\"normal\"},")
                // language
                + "{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P407\",\"hash\":\"jw26z0hw2bocyknsulftmhbqrnjfmybohryrolfg\",\"datavalue\":{\"type\":\"wikibase-entityid\",\"value\":{\"entity-type\":\"item\",\"numeric-id\":\"7737\",\"id\":\"Q7737\"}},\"datatype\":\"wikibase-item\"},\"type\":\"statement\",\"rank\":\"normal\"},"
                // author
                + "{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P50\",\"hash\":\"jw26z0hw4boykizmhlyuobhtdmdggwmuidmtdstr\",\"datavalue\":{\"type\":\"wikibase-entityid\",\"value\":{\"entity-type\":\"item\",\"numeric-id\":\"218295\",\"id\":\"Q218295\"}},\"datatype\":\"wikibase-item\"},\"type\":\"statement\",\"rank\":\"normal\"},"
                // published in
                + "{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P1433\",\"hash\":\"jw26z0hw6bounqvtvgpvhoncikmtldjxckajmiym\",\"datavalue\":{\"type\":\"wikibase-entityid\",\"value\":{\"entity-type\":\"item\",\"numeric-id\":\"4426104\",\"id\":\"Q4426104\"}},\"datatype\":\"wikibase-item\"},\"type\":\"statement\",\"rank\":\"normal\"},"
                // title
                + "{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P1476\",\"hash\":\"jw26z0hw8bogkqwsnbipsbinshmpsnprjdryvkvz\",\"datavalue\":{\"type\":\"monolingualtext\",\"value\":{\"language\":\"ru\",\"text\":\""
                + StringEscapeUtils.escapeJson(summary)
                + "\"}},\"datatype\":\"monolingualtext\"},\"type\":\"statement\",\"rank\":\"normal\"},"
                // inception
                + "{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P571\",\"hash\":\"jw26z0hwabosmbjagxozmqzmxslolincdefyhrdu\",\"datavalue\":{\"type\":\"time\",\"value\":{\"time\":\"+"
                + DateFormatUtils.ISO_8601_EXTENDED_DATE_FORMAT.format(docDate)
                + "T00:00:00Z\",\"timezone\":0,\"before\":0,\"after\":0,\"precision\":11,\"calendarmodel\":\"http://www.wikidata.org/entity/Q1985727\"}},\"datatype\":\"time\"},\"type\":\"statement\",\"rank\":\"normal\"},"
                // series ordinal
                + ("{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P1545\",\"hash\":\"jw26z0hwcboxassfjzdcoyzlxcxihkudlsrfrsmt\",\"datavalue\":{\"type\":\"string\",\"value\":\""
                        + StringEscapeUtils.escapeJson(docNumber)
                        + "\"},\"datatype\":\"string\"},\"type\":\"statement\",\"rank\":\"normal\"},")
                + "{\"mainsnak\":{\"snaktype\":\"value\",\"property\":\"P953\",\"hash\":\"jw26z0hwebpvogxtdzhmyvfqfrkdfefrhtueaenh\",\"datavalue\":{\"type\":\"string\",\"value\":\""
                + StringEscapeUtils.escapeJson(url.toString())
                + "\"},\"datatype\":\"url\"},\"type\":\"statement\",\"rank\":\"normal\"}],"
                + "\"sitelinks\":{\"ruwikisource\":{\"site\":\"ruwikisource\",\"title\":\""
                + articleTitle.replaceAll(" ", "_") + "\",\"badges\":[]}}}");

        return toUpload;
    }

    private String processTextContent(String content) {
        content = Pattern.compile("([^ ])\\n     (.)", Pattern.MULTILINE).matcher(content).replaceAll("$1\n\n$2");
        content = Pattern.compile("([^ \n\\}])\\n([^\n \\{])", Pattern.MULTILINE).matcher(content).replaceAll("$1 $2");

        final Pattern whitespacePattern = Pattern.compile("([^\n ])  ");
        while (whitespacePattern.matcher(content).find()) {
            content = whitespacePattern.matcher(content).replaceAll("$1 ");
        }

        // splitted headers
        content = content.replaceAll("\\n[ ]{1,}([\"А-Я\\- ]+)\\n\\n[ ]{1,}([А-ЯIVX][\"IVXА-Я\\- ]+)\\n",
                "\n      $1 $2\n");

        // headers
        content = content.replaceAll("\\n[ ]*([\"А-ЯIVX\\- ]+)\\n", "\n=== $1 ===\n");

        // awards
        content = replaceAll(content,
                "\\n=== [«\"]ЗАСЛУЖЕННЫЙ (.*) РОССИЙСКОЙ ФЕДЕРАЦИИ[»\"] ===\n",
                (matcher, g1) -> "\n=== [[:w:ru:Заслуженный " + g1.toLowerCase() + " Российской Федерации|«ЗАСЛУЖЕННЫЙ "
                        + g1 + " {{nobr|РОССИЙСКОЙ ФЕДЕРАЦИИ}}»]] ===\n");

        for (String years : new String[] { "XX", "XXV", "XXX", "XL", "L" })
            content = wikilinkHeaders(content,
                    "ЗНАКОМ ОТЛИЧИЯ \"ЗА БЕЗУПРЕЧНУЮ СЛУЖБУ\" " + years + " ЛЕТ",
                    "Знак отличия «За безупречную службу» " + years + " лет");

        content = wikilinkHeaders(content, "МЕДАЛЬЮ НЕСТЕРОВА", "Медаль Нестерова");
        content = wikilinkHeaders(content,
                "МЕДАЛЬЮ ОРДЕНА \"ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ\" I СТЕПЕНИ",
                "Медаль ордена «За заслуги перед Отечеством» I степени");
        content = wikilinkHeaders(content,
                "МЕДАЛЬЮ ОРДЕНА \"ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ\" II СТЕПЕНИ",
                "Медаль ордена «За заслуги перед Отечеством» II степени");
        content = wikilinkHeaders(content, "МЕДАЛЬЮ \"ЗА СПАСЕНИЕ ПОГИБАВШИХ\"", "Медаль «За спасение погибавших»");

        content = wikilinkHeaders(content, "ОРДЕНОМ АЛЕКСАНДРА НЕВСКОГО", "Орден Александра Невского (Россия)");
        content = wikilinkHeaders(content, "ОРДЕНОМ ДРУЖБЫ", "Орден Дружбы (Россия)");
        content = wikilinkHeaders(content, "ОРДЕНОМ МУЖЕСТВА", "Орден Мужества");
        content = wikilinkHeaders(content, "ОРДЕНОМ ПОЧЕТА", "Орден Почёта (Россия)");
        for (String ledge : new String[] { "I", "II", "III", "IV" })
            content = wikilinkHeaders(content,
                    "ОРДЕНОМ \"ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ\" " + ledge + " СТЕПЕНИ",
                    "Орден «За заслуги перед Отечеством» " + ledge + " степени");

        content = wikilinkHeaders(content, "ОРДЕНОМ \"РОДИТЕЛЬСКАЯ СЛАВА\"", "Орден «Родительская слава»");

        // wikify
        content = Wikify.wikify(content);
        return content;
    }

    private void importFrom(URI originalUrl) throws Exception {

        int page = 1;
        URI nextUrl = originalUrl;

        String html, title = null, summary = null, content = null;
        do {
            html = restTemplate.getForEntity(nextUrl, String.class).getBody();

            if (page == 1) {
                title = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html, "<h1>", "</h1>"));
                summary = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html,
                        "<div class=\"read__lead entry-summary p-summary\" itemprop=\"description\">",
                        "</div>"));
                content = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html, "<pre>", "</pre>"));

                assertNotBlank(title);
                assertNotBlank(summary);
                assertNotBlank(content);
            } else {
                content += "\n";
                final String nextPageContent =
                        StringUtils.substringBetween(html, "<pre class=\"masha-ignore\">", "</pre>");
                assertNotBlank(nextPageContent);
                content += StringEscapeUtils.unescapeHtml4(nextPageContent);
            }
            nextUrl = URI.create(originalUrl + "/page/" + (++page));
        } while (html.contains("Показать следующую страницу документа"));

        ToUpload toUpload;
        if (title.startsWith("Указ Президента Российской Федерации")) {
            toUpload = importAsУказ(originalUrl, title, summary, content);
        } else if (title.startsWith("Федеральный конституционный закон от ")) {
            return;
        } else if (title.startsWith("Федеральный закон от ")) {
            return;
        } else {
            throw new AssertionError("Unsupported: \"" + title + "\" from " + originalUrl);
        }

        if (!prompt("Agree to upload? ")) {
            throw new RuntimeException("Aborted");
        }

        final String editSummary = "Importing from " + originalUrl;
        wikisourceApiHelper.edit(editSummary, toUpload.getWikisourceTitle(), toUpload.getWikisourceContent());
        wikidataApiHelper.wbeditentity_new(editSummary, toUpload.getWikidataData());
    }

    private static Boolean prompt(String prmpt) {
        List<String> allowed =
                Arrays.asList(Boolean.FALSE.toString().toLowerCase(), Boolean.TRUE.toString().toLowerCase());
        String word = "";
        do {
            System.out.print(prmpt);
            Scanner scan = new Scanner(System.in);
            if (scan.hasNextLine()) {
                word = scan.nextLine().toLowerCase().trim();
            }
        } while (!allowed.contains(word)); // no need for == true
        return Boolean.valueOf(word);
    }

    public void run() throws Exception {
        for (int pageId = 18280; pageId <= 20000; pageId++) {
            importFrom(URI.create("http://www.kremlin.ru/acts/bank/" + pageId));
        }
    }

}
