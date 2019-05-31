package io.github.vlsergey.kremlin2wikisource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CategoryAdder {

	private static final String[] TO_UPDATE = ("Указ Президента РФ от 02.09.2002 № 936\r\n"
			+ "Указ Президента РФ от 02.09.2002 № 937\r\n" + "Указ Президента РФ от 02.09.2002 № 938\r\n"
			+ "Указ Президента РФ от 02.09.2002 № 939\r\n" + "Указ Президента РФ от 02.09.2002 № 940\r\n"
			+ "Указ Президента РФ от 02.09.2002 № 941\r\n" + "Указ Президента РФ от 02.09.2002 № 942\r\n"
			+ "Указ Президента РФ от 02.09.2002 № 943\r\n" + "Указ Президента РФ от 05.09.2002 № 947\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 948\r\n" + "Указ Президента РФ от 05.09.2002 № 949\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 950\r\n" + "Указ Президента РФ от 05.09.2002 № 951\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 952\r\n" + "Указ Президента РФ от 05.09.2002 № 953\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 954\r\n" + "Указ Президента РФ от 05.09.2002 № 955\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 956\r\n" + "Указ Президента РФ от 05.09.2002 № 957\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 958\r\n" + "Указ Президента РФ от 05.09.2002 № 959\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 960\r\n" + "Указ Президента РФ от 05.09.2002 № 961\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 962\r\n" + "Указ Президента РФ от 05.09.2002 № 963\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 964\r\n" + "Указ Президента РФ от 05.09.2002 № 965\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 966\r\n" + "Указ Президента РФ от 05.09.2002 № 967\r\n"
			+ "Указ Президента РФ от 05.09.2002 № 968\r\n" + "Указ Президента РФ от 21.09.2002 № 1028\r\n"
			+ "Указ Президента РФ от 21.09.2002 № 1029\r\n" + "Указ Президента РФ от 21.09.2002 № 1030\r\n"
			+ "Указ Президента РФ от 22.08.2002 № 912\r\n" + "Указ Президента РФ от 22.08.2002 № 913\r\n"
			+ "Указ Президента РФ от 22.08.2002 № 914\r\n" + "Указ Президента РФ от 22.08.2002 № 915\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1039\r\n" + "Указ Президента РФ от 23.09.2002 № 1040\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1041\r\n" + "Указ Президента РФ от 23.09.2002 № 1042\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1043\r\n" + "Указ Президента РФ от 23.09.2002 № 1044\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1045\r\n" + "Указ Президента РФ от 23.09.2002 № 1046\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1047\r\n" + "Указ Президента РФ от 23.09.2002 № 1048\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1049\r\n" + "Указ Президента РФ от 23.09.2002 № 1050\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1051\r\n" + "Указ Президента РФ от 23.09.2002 № 1052\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1053\r\n" + "Указ Президента РФ от 23.09.2002 № 1054\r\n"
			+ "Указ Президента РФ от 23.09.2002 № 1055\r\n" + "Указ Президента РФ от 23.09.2002 № 1056\r\n"
			+ "Указ Президента РФ от 24.09.2002 № 1058\r\n" + "Указ Президента РФ от 24.09.2002 № 1059\r\n"
			+ "Указ Президента РФ от 24.09.2002 № 1060\r\n" + "Указ Президента РФ от 24.09.2002 № 1061\r\n"
			+ "Указ Президента РФ от 24.09.2002 № 1062\r\n" + "Указ Президента РФ от 24.09.2002 № 1063\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1071\r\n" + "Указ Президента РФ от 25.09.2002 № 1072\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1073\r\n" + "Указ Президента РФ от 25.09.2002 № 1074\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1075\r\n" + "Указ Президента РФ от 25.09.2002 № 1076\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1077\r\n" + "Указ Президента РФ от 25.09.2002 № 1078\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1079\r\n" + "Указ Президента РФ от 25.09.2002 № 1080\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1081\r\n" + "Указ Президента РФ от 25.09.2002 № 1082\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1083\r\n" + "Указ Президента РФ от 25.09.2002 № 1084\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1085\r\n" + "Указ Президента РФ от 25.09.2002 № 1086\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1087\r\n" + "Указ Президента РФ от 25.09.2002 № 1088\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1089\r\n" + "Указ Президента РФ от 25.09.2002 № 1090\r\n"
			+ "Указ Президента РФ от 25.09.2002 № 1091\r\n" + "Указ Президента РФ от 25.09.2002 № 1092")
					.split("[\r\n]");

	@Autowired
	private WikisourceApiHelper wikisourceApiHelper;

	public void run() throws Exception {
		for (String article : TO_UPDATE) {
			if (article.trim().isEmpty())
				continue;

			wikisourceApiHelper.append("Add [[Категория:Указы Президента РФ о помиловании]]",
					article.trim(), "\n\n[[Категория:Указы Президента РФ о помиловании]]");
		}
	}

}
