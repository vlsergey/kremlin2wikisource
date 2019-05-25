package io.github.vlsergey.kremlin2wikisource;

import java.util.Collections;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

@Component
@Slf4j
public class WikidataApiHelper extends MediaWikiApiHelper {

    static final String API_ENDPOINT = "https://www.wikidata.org/w/api.php";

    public WikidataApiHelper() {
        super(API_ENDPOINT);
    }

    public void wbeditentity_new(String summary, String data) throws Exception {
        String csrfToken = fetchCsrfToken();

        log.info("Posting new entity...");
        HttpHeaders requestHeaders = new HttpHeaders();
        requestHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

        MultiValueMap<String, String> editArgs = new LinkedMultiValueMap<>();
        editArgs.put("action", Collections.singletonList("wbeditentity"));
        editArgs.put("format", Collections.singletonList("json"));
        editArgs.put("assert", Collections.singletonList("user"));
        editArgs.put("summary", Collections.singletonList(summary));
        editArgs.put("data", Collections.singletonList(data));
        editArgs.put("new", Collections.singletonList("item"));
        editArgs.put("token", Collections.singletonList(csrfToken));

        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(editArgs, requestHeaders);

        final ResponseEntity<String> editResponse = restTemplate.postForEntity(this.apiEndpoint, request, String.class);
        log.info("wbeditentity response: " + editResponse);
        assertOk(editResponse);
    }

}
