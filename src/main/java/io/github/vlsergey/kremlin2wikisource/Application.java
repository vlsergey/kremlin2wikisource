package io.github.vlsergey.kremlin2wikisource;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Import;

@SpringBootApplication
@Import({ RestTemplateConfig.class })
public class Application {

	private static final String LGNAME = "Vlsergey-at-work@kremlin2wikisource";
	private static final String LGPASSWORD = System.getProperty("lgpassword");

	public static void main(String[] args) throws Exception {
		try (final ConfigurableApplicationContext context = SpringApplication.run(Application.class, args)) {

			context.getBean(WikisourceApiHelper.class).login(LGNAME, LGPASSWORD);
			context.getBean(WikidataApiHelper.class).login(LGNAME, LGPASSWORD);

			// context.getBean(CategoryAdder.class).run();
			context.getBean(Importer.class).run();
		}
	}

}
