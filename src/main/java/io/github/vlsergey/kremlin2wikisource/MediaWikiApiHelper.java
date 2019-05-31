package io.github.vlsergey.kremlin2wikisource;

import io.github.vlsergey.kremlin2wikisource.wikiapi.QueryTokensResponse;
import java.net.URI;
import java.util.Collections;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

@Slf4j
public class MediaWikiApiHelper {

	protected static void assertOk(final ResponseEntity<?> apiResponse) throws AssertionError {
		if (!apiResponse.getStatusCode().is2xxSuccessful()) {
			throw new AssertionError();
		}
		if (apiResponse.getHeaders().containsKey("MediaWiki-API-Error")) {
			throw new RuntimeException(
					"MediaWiki API Error: " + apiResponse.getHeaders().getFirst("MediaWiki-API-Error"));
		}
	}

	protected final String apiEndpoint;

	@Autowired
	protected RestTemplate restTemplate;

	public MediaWikiApiHelper(final String apiEndpoint) {
		this.apiEndpoint = apiEndpoint;
	}

	public void edit(String summary, String title, String content) throws Exception {
		String csrfToken = fetchCsrfToken();

		log.info("Do edit...");

		HttpHeaders requestHeaders = new HttpHeaders();
		requestHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		MultiValueMap<String, String> editArgs = new LinkedMultiValueMap<>();
		editArgs.add("action", "edit");
		editArgs.add("title", title);
		editArgs.add("text", content);
		editArgs.add("summary", summary);
		editArgs.add("token", csrfToken);
		editArgs.add("format", "json");
		editArgs.add("assert", "user");

		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(editArgs, requestHeaders);

		final ResponseEntity<String> editResponse = restTemplate.postForEntity(this.apiEndpoint, request, String.class);
		log.info("Edit response: " + editResponse);
		assertOk(editResponse);
	}

	public void append(String summary, String title, String content) throws Exception {
		String csrfToken = fetchCsrfToken();

		log.info("Do edit...");

		HttpHeaders requestHeaders = new HttpHeaders();
		requestHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		MultiValueMap<String, String> editArgs = new LinkedMultiValueMap<>();
		editArgs.add("action", "edit");
		editArgs.add("title", title);
		editArgs.add("appendtext", content);
		editArgs.add("summary", summary);
		editArgs.add("token", csrfToken);
		editArgs.add("format", "json");
		editArgs.add("assert", "user");

		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(editArgs, requestHeaders);

		final ResponseEntity<String> editResponse = restTemplate.postForEntity(this.apiEndpoint, request, String.class);
		log.info("Edit response: " + editResponse);
		assertOk(editResponse);
	}

	protected String fetchCsrfToken() throws Exception {
		log.info("Fetching CSRF token...");

		final ResponseEntity<QueryTokensResponse> resp = restTemplate.getForEntity(
				URI.create(this.apiEndpoint + "?action=query&meta=tokens&type=csrf&format=json&assert=user"),
				QueryTokensResponse.class);
		assertOk(resp);
		log.info("Fetching CSRF token... Done: {}", resp);
		final String csrftoken = resp.getBody().getQuery().getTokens().getCsrftoken();
		log.info("Fetching CSRF token... Done: {}", csrftoken);
		return csrftoken;
	}

	private String fetchLgToken() throws AssertionError {
		log.info("Fetching login token...");
		final ResponseEntity<QueryTokensResponse> lgTokenResponse = restTemplate.getForEntity(
				this.apiEndpoint + "?action=query&meta=tokens&type=login&format=json", QueryTokensResponse.class);
		if (!lgTokenResponse.getStatusCode().is2xxSuccessful()) {
			throw new AssertionError();
		}
		return lgTokenResponse.getBody().getQuery().getTokens().getLogintoken();
	}

	public void login(String lgname, String lgpassword) throws AssertionError {
		String lgtoken = fetchLgToken();

		log.info("Do login...");

		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
		map.put("action", Collections.singletonList("login"));
		map.put("lgname", Collections.singletonList(lgname));
		map.put("lgpassword", Collections.singletonList(lgpassword));
		map.put("lgtoken", Collections.singletonList(lgtoken));
		map.put("format", Collections.singletonList("json"));

		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(map, headers);

		final ResponseEntity<String> loginResponse = restTemplate.postForEntity(this.apiEndpoint, request,
				String.class);
		log.info("Do login... {}", loginResponse);
		if (!loginResponse.getStatusCode().is2xxSuccessful()) {
			throw new AssertionError();
		}
		log.info("Do login... Done");
	}

}