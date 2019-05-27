package io.github.vlsergey.kremlin2wikisource;

import java.net.URI;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.SneakyThrows;

@Component
public class WikidataModelHelper {

	public static final int MAX_DESCRIPTION_LENGTH = 250;
	
	private static class Claim extends LinkedHashMap<String, Object> {
		private static final long serialVersionUID = 1L;
	}

	private static class Datavalue extends LinkedHashMap<String, Object> {
		private static final long serialVersionUID = 1L;
	}

	private static class Snak extends LinkedHashMap<String, Object> {
		private static final long serialVersionUID = 1L;
	}

	private static class Value extends LinkedHashMap<String, Object> {
		private static final long serialVersionUID = 1L;
	}

	private static AtomicLong hashCounter = new AtomicLong(1);

	@Autowired
	private ObjectMapper objectMapper;

	@SneakyThrows
	private String toClaim(int propertyId, Supplier<Snak> snakSuplier) {
		Claim claim = new Claim();
		claim.put("mainsnak", snakSuplier.get());
		claim.put("type", "statement");
		claim.put("rank", "normal");
		return objectMapper.writeValueAsString(claim);
	}

	@SneakyThrows
	private Datavalue toDatavalue(String datavalueType, String value) {
		Datavalue datavalue = new Datavalue();
		datavalue.put("type", datavalueType);
		datavalue.put("value", value);
		return datavalue;
	}

	@SneakyThrows
	private Datavalue toDatavalue(String datavalueType, Supplier<Value> valueSupplier) {
		Datavalue datavalue = new Datavalue();
		datavalue.put("type", datavalueType);
		datavalue.put("value", valueSupplier.get());
		return datavalue;
	}

	@SneakyThrows
	public String toLabelalike(String languageCode, String value) {
		Map<String, Object> byCode = new LinkedHashMap<>();
		byCode.put("language", languageCode);
		if (value.length() > MAX_DESCRIPTION_LENGTH) {
			byCode.put("value", value.substring(0, MAX_DESCRIPTION_LENGTH - 3) + "…");
		} else {
			byCode.put("value", value);
		}

		Map<String, Object> labels = new LinkedHashMap<>();
		labels.put(languageCode, byCode);

		return objectMapper.writeValueAsString(labels);
	}

	@SneakyThrows
	public String toMonolingualClaim(final int propertyId, final String language, final String text) {
		return toClaim(propertyId, () -> //
		toSnak(propertyId, "monolingualtext", () -> //
		toDatavalue("monolingualtext", () -> //
		toMonolingualtextValue(language, text))));
	}

	@SneakyThrows
	private Value toMonolingualtextValue(String language, String text) {
		Value value = new Value();
		value.put("language", language);
		value.put("text", text);
		return value;
	}

	@SneakyThrows
	public String toSitelinks(String site, String title) {
		Map<String, Object> byProject = new LinkedHashMap<>();
		byProject.put("site", site);
		byProject.put("title", title);

		Map<String, Object> sitelinks = new LinkedHashMap<>();
		sitelinks.put(site, byProject);
		return objectMapper.writeValueAsString(sitelinks);
	}

	@SneakyThrows
	private Snak toSnak(int propertyId, String snakDatatype, Supplier<Map<String, Object>> datavalueSupplier) {
		Snak snak = new Snak();
		snak.put("snaktype", "value");
		snak.put("property", "P" + propertyId);
		snak.put("hash", Long.toString(hashCounter.getAndIncrement(), Character.MAX_RADIX));
		snak.put("datavalue", datavalueSupplier.get());
		snak.put("datatype", snakDatatype);
		return snak;
	}

	@SneakyThrows
	public String toStringClaim(final int propertyId, final String value) {
		return toClaim(propertyId, () -> //
		toSnak(propertyId, "string", () -> //
		toDatavalue("string", value)));
	}

	@SneakyThrows
	public String toTimeClaim(final int propertyId, final Date date) {
		return toClaim(propertyId, () -> //
		toSnak(propertyId, "time", () -> //
		toDatavalue("time", () -> //
		toTimeValue(date))));
	}

	@SneakyThrows
	private Value toTimeValue(Date date) {
		Value value = new Value();
		value.put("time", "+" + DateFormatUtils.ISO_8601_EXTENDED_DATE_FORMAT.format(date) + "T00:00:00Z");
		value.put("timezone", 0);
		value.put("before", 0);
		value.put("after", 0);
		value.put("precision", 11);
		value.put("calendarmodel", "http://www.wikidata.org/entity/Q1985727");
		return value;
	}

	@SneakyThrows
	public String toUrlClaim(final int propertyId, final URI uri) {
		return toClaim(propertyId, () -> //
		toSnak(propertyId, "url", () -> //
		toDatavalue("string", uri.toString())));
	}

	@SneakyThrows
	public String toWikibaseEntityidClaim(final int propertyId, final int numericId) {
		return toClaim(propertyId, () -> //
		toSnak(propertyId, "wikibase-item", () -> //
		toDatavalue("wikibase-entityid", () -> //
		toWikibaseEntityidValue(numericId))));
	}

	@SneakyThrows
	private Value toWikibaseEntityidValue(int numericId) {
		Value value = new Value();
		value.put("entity-type", "item");
		value.put("numeric-id", "" + numericId);
		value.put("id", "Q" + numericId);
		return value;
	}

}
