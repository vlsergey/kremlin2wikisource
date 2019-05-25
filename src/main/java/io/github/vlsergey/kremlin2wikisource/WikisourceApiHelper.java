package io.github.vlsergey.kremlin2wikisource;

import org.springframework.stereotype.Component;

@Component
public class WikisourceApiHelper extends MediaWikiApiHelper {

    static final String API_ENDPOINT = "https://ru.wikisource.org/w/api.php";

    public WikisourceApiHelper() {
        super(API_ENDPOINT);
    }

}
