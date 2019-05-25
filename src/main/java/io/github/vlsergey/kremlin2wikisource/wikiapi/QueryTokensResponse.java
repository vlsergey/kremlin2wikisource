package io.github.vlsergey.kremlin2wikisource.wikiapi;

import lombok.Data;

@Data
public class QueryTokensResponse {

    @Data
    public static class Query {

        @Data
        public static class Tokens {

            private String csrftoken;

            private String logintoken;

            private String patroltoken;

            private String watchtoken;
        }

        private Tokens tokens;

    }

    private String batchcomplete;

    private Query query;
}
