---
title:    "Java: Unterstrings extrahieren"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum?
Viele Menschen stehen vor der Herausforderung, Teilstrings aus längeren Texten oder Strings extrahieren zu müssen. Dies kann aus verschiedenen Gründen nützlich sein, wie zum Beispiel die Analyse von Daten oder die Extraktion von Informationen aus Benutzereingaben. In dieser Blogpost werden wir uns mit verschiedenen Techniken für das Extrahieren von Substrings in Java befassen.

## Wie geht man vor?
Um Teilstrings aus einem längeren String zu extrahieren, gibt es in Java verschiedene Methoden und Techniken. Wir werden uns die häufigsten und nützlichsten davon ansehen, um Ihnen einen Einstieg in das Extrahieren von Substrings zu geben.

### Substring-Methode
Die einfachste Methode zum Extrahieren von Teilstrings in Java ist die Verwendung der `substring()`-Methode. Diese Methode ermöglicht es uns, einen Teilstring basierend auf einem Start- und Endindex aus einem längeren String zu extrahieren. Hier ist ein Beispielcode, der dies verdeutlicht:

```Java
String text = "Hallo Welt";
String sub = text.substring(2, 5);
System.out.println(sub);
```

Ausgegeben wird dabei "llo" sein, da die Zählung der Indizes bei 0 beginnt und der Teilstring vom dritten (Index 2) bis zum fünften Index (exklusiv) extrahiert wird.

### String-Tokenizer-Klasse
Eine andere Möglichkeit, Teilstrings zu extrahieren, ist die Verwendung der `StringTokenizer`-Klasse. Diese ermöglicht es uns, einen String basierend auf einem bestimmten Trennzeichen in mehrere Teilstrings aufzuteilen. Hier ist ein Beispielcode:

```Java
String text = "Dies, ist, ein, Beispiel";
StringTokenizer st = new StringTokenizer(text, ",");
while (st.hasMoreTokens()) {
    String token = st.nextToken();
    System.out.println(token);
}
```

Die Ausgabe wäre dabei:

- Dies
- ist
- ein
- Beispiel

## Tiefentauchen
Zusätzlich zu den oben genannten Methoden gibt es auch noch andere Techniken für das Extrahieren von Teilstrings in Java. Zum Beispiel können Sie die `indexOf()`-Methode verwenden, um das Auftreten eines bestimmten Zeichens oder einer Zeichenfolge in einem String zu finden, und anschließend die `substring()`-Methode verwenden, um den Teilstring zu extrahieren.

Es ist auch wichtig zu beachten, dass die `substring()`-Methode immer eine neue String-Instanz zurückgibt und nicht den ursprünglichen String ändert. Wenn Sie also den ursprünglichen String ändern möchten, können Sie die `replace()`-Methode verwenden. Weitere Informationen zu diesen Techniken finden Sie in der offiziellen Java-Dokumentation.

## Siehe auch
- [Java Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Tutorialspoint-Artikel über das Extrahieren von Substrings in Java](https://www.tutorialspoint.com/java/java_string_substring.htm)
- [GeeksforGeeks-Artikel über die String-Tokenizer-Klasse](https://www.geeksforgeeks.org/java-stringtokenizer-class-java/)

Vielen Dank fürs Lesen und viel Spaß beim Extrahieren von Substrings in Java!