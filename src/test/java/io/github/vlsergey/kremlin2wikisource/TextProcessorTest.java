package io.github.vlsergey.kremlin2wikisource;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

@SuppressWarnings("deprecation")
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = { TextProcessor.class, Wikify.class, WikidataModelHelper.class, ObjectMapper.class })
public class TextProcessorTest {

	@Autowired
	private TextProcessor textProcessor;

	private void testAsРаспоряжение(final int bankActNumber, final String title, final String summary)
			throws Exception {
		String src = IOUtils.toString(
				TextProcessorTest.class.getResource("/ru/kremlin/acts/bank/" + bankActNumber + ".txt").toURI(),
				StandardCharsets.UTF_8);
		src = Importer.prepareHtml(src);
		src = StringEscapeUtils.unescapeHtml4(src);

		String actual = textProcessor
				.importAsРаспоряжение(URI.create("http://www.kremlin.ru/acts/bank/" + bankActNumber), title, summary,
						src)
				.getWikisourceContent().trim()
		// .replace('\u00a0', '·')
		;

		String expected = IOUtils.toString(
				TextProcessorTest.class.getResource("/ru/kremlin/acts/bank/" + bankActNumber + ".wiki").toURI(),
				StandardCharsets.UTF_8).replace("\r", "").trim();

		Assert.assertEquals(expected, actual);
	}

	private void testAsУказ(final int bankActNumber, final String title, final String summary) throws Exception {
		String src = IOUtils.toString(
				TextProcessorTest.class.getResource("/ru/kremlin/acts/bank/" + bankActNumber + ".txt").toURI(),
				StandardCharsets.UTF_8);
		src = Importer.prepareHtml(src);
		src = StringEscapeUtils.unescapeHtml4(src);

		String actual = textProcessor
				.importAsУказ(URI.create("http://www.kremlin.ru/acts/bank/" + bankActNumber), title, summary, src)
				.getWikisourceContent().trim();

		String expected = IOUtils.toString(
				TextProcessorTest.class.getResource("/ru/kremlin/acts/bank/" + bankActNumber + ".wiki").toURI(),
				StandardCharsets.UTF_8).replace("\r", "").trim();

		Assert.assertEquals(expected, actual);
	}

	@Test
	public void test18376() throws Exception {
		testAsРаспоряжение(18376, "Распоряжение Президента Российской Федерации от 05.08.2002 г. № 369-рп",
				"Об изменении состава Совета по взаимодействию с религиозными объединениями при Президенте Российской Федерации");
	}

	@Test
	public void test18434() throws Exception {
		testAsУказ(18434, "Указ Президента Российской Федерации от 25.07.2002 г. № 780",
				"О внесении изменений в Положение о порядке выдачи удостоверения судьи судьям Конституционного Суда Российской Федерации, "
						+ "Верховного Суда Российской Федерации, "
						+ "Высшего Арбитражного Суда Российской Федерации и судьям, "
						+ "назначаемым Президентом Российской Федерации, "
						+ "утвержденное Указом Президента Российской Федерации от 9 сентября 2000 г. N 1624");
	}

	@Test
	public void test18435() throws Exception {
		testAsУказ(18435, "Указ Президента Российской Федерации от 25.07.2002 г. № 778",
				"О внесении изменений в отдельные акты Президента Российской Федерации");
	}

	@Test
	public void test18440() throws Exception {
		testAsУказ(18440, "Указ Президента Российской Федерации от 29.07.2002 г. № 798",
				"О внесении изменений в Указ Президента Российской Федерации от 10 апреля 2000 г. N 660 «О призыве офицеров запаса на военную службу в 2000–2005 годах»");
	}

	@Test
	public void test18441() throws Exception {
		testAsУказ(18441, "Указ Президента Российской Федерации от 31.07.2002 г. № 817",
				"О предоставлении права на получение отсрочки от призыва на военную службу отдельным категориям граждан Российской Федерации");
	}

	@Test
	public void test18447() throws Exception {
		testAsРаспоряжение(18447, "Распоряжение Президента Российской Федерации от 01.08.2002 г. № 359-рп", "");
	}

	@Test
	public void test18454() throws Exception {
		testAsУказ(18454, "Указ Президента Российской Федерации от 06.08.2002 г. № 859",
				"О награждении государственными наградами Российской Федерации военнослужащих Вооруженных Сил Российской Федерации");
	}

	@Test
	public void test18458() throws Exception {
		testAsУказ(18458, "Указ Президента Российской Федерации от 12.08.2002 г. № 885",
				"Об утверждении общих принципов служебного поведения государственных служащих");
	}

	@Test
	public void test18460() throws Exception {
		testAsРаспоряжение(18460, "Распоряжение Президента Российской Федерации от 17.08.2002 г. № 385-рп", "");
	}

	@Test
	public void test18472() throws Exception {
		testAsРаспоряжение(18472, "Распоряжение Президента Российской Федерации от 30.08.2002 г. № 391-рп",
				"О направлении сотрудников органов внутренних дел Российской Федерации в состав Миссии ООН в Демократической Республике Конго");
	}

	@Test
	public void test18476() throws Exception {
		testAsУказ(18476, "Указ Президента Российской Федерации от 02.09.2002 г. № 944",
				"О признании утратившим силу Указа Президента Российской Федерации от 14 октября 1992 г. N 1230 «О регулировании арендных отношений и приватизации имущества государственных и муниципальных предприятий, сданного в аренду»");
	}

	@Test
	public void test18486() throws Exception {
		testAsРаспоряжение(18486, "Распоряжение Президента Российской Федерации от 02.09.2002 г. № 402-рп",
				"О представителях ГТК России в Федеративной Республике Германия и Королевстве Бельгия");
	}

	@Test
	public void test18517() throws Exception {
		testAsУказ(18517, "Указ Президента Российской Федерации от 09.09.2002 г. № 974",
				"О внесении дополнений в некоторые акты Президента Российской Федерации");
	}

	@Test
	public void test18527() throws Exception {
		testAsУказ(18527, "Указ Президента Российской Федерации от 16.09.2002 г. № 993",
				"О внесении изменений и дополнений в Положение о Министерстве иностранных дел Российской Федерации");
	}

	@Test
	public void test18529() throws Exception {
		testAsУказ(18529, "Указ Президента Российской Федерации от 16.09.2002 г. № 988",
				"О признании утратившими силу некоторых указов Президента Российской Федерации");
	}

	@Test
	public void test18560() throws Exception {
		testAsУказ(18560, "Указ Президента Российской Федерации от 21.09.2002 г. № 1011",
				"Вопросы Министерства Российской Федерации по делам гражданской обороны, "
						+ "чрезвычайным ситуациям и ликвидации последствий стихийных бедствий");
	}

	@Test
	public void test18574() throws Exception {
		testAsРаспоряжение(18574, "Распоряжение Президента Российской Федерации от 23.09.2002 г. № 450-рп", "");
	}

	@Test
	public void test18588() throws Exception {
		testAsУказ(18588, "Указ Президента Российской Федерации от 23.09.2002 г. № 1036",
				"О награждении орденом «За военные заслуги» Гужвина А.П.");
	}

	@Test
	public void test18643() throws Exception {
		testAsУказ(18643, "Указ Президента Российской Федерации от 03.10.2002 г. № 1112",
				"О временном исполнении обязанностей губернатора Красноярского края");
	}

	@Test
	public void test18649() throws Exception {
		testAsУказ(18649, "Указ Президента Российской Федерации от 04.10.2002 г. № 1120",
				"Об утверждении Временного положения о военных комендатурах, дислоцированных на территории Чеченской Республики");
	}

	@Test
	public void test18656() throws Exception {
		testAsРаспоряжение(18656, "Распоряжение Президента Российской Федерации от 08.10.2002 г. № 484-рп",
				"О заключении Соглашения между Российской Федерацией и Республикой Таджикистан "
						+ "о порядке передачи Республике Таджикистан участка государственной границы "
						+ "Республики Таджикистан с Китайской Народной Республикой, "
						+ "охраняемого Пограничной службой Российской Федерации, и имущества, "
						+ "используемого Пограничной службой Российской Федерации");
	}

	@Test
	public void test18770() throws Exception {
		testAsУказ(18770, "Указ Президента Российской Федерации от 24.10.2002 г. № 1257",
				"О награждении орденом Дружбы");
	}

	@Test
	public void test18777() throws Exception {
		testAsУказ(18777, "Указ Президента Российской Федерации от 25.10.2002 г. № 1261",
				"О назначении судей верховных судов республик, " + "Краснодарского краевого суда, "
						+ "областных судов, " + "Московского городского суда и судов автономных округов");
	}

	@Test
	public void test18786() throws Exception {
		testAsУказ(18786, "Указ Президента Российской Федерации от 28.10.2002 г. № 1264",
				"О награждении государственными наградами Российской Федерации "
						+ "участников кругосветного путешествия на яхте «Апостол Андрей»");
	}

}
