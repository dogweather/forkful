---
title:                "Java: Zeichen löschen, die einem Muster entsprechen."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum
In der Programmierung gibt es oft Situationen, in denen wir bestimmte Zeichen aus einem Text entfernen möchten. Dies kann zum Beispiel bei der Datenbereinigung oder beim Validieren von Eingaben erforderlich sein. In diesem Artikel werden wir uns genauer ansehen, wie wir in Java Zeichen entfernen können, die einem bestimmten Muster entsprechen.

# Wie geht das?
Um Zeichen in Java zu entfernen, können wir die `replaceAll()` Methode nutzen. Diese Methode akzeptiert zwei Parameter - das Muster, nach dem gesucht werden soll und den Text, in dem die Zeichen entfernt werden sollen.

```Java
String text = "Dies ist ein Beispieltext!";
String pattern = "e";

String result = text.replaceAll(pattern, "");

System.out.println(result); // gibt "Dis ist in Bispltext!" aus
```

In diesem Beispiel haben wir das Zeichen "e" aus dem Text entfernt, indem wir es durch einen leeren String ersetzt haben. Wir können jedoch auch andere Muster wie reguläre Ausdrücke verwenden, um spezifischere Zeichen zu entfernen.

```Java
String text = "Das ist eine Zahl: 12345!";
String pattern = "\\D";

String result = text.replaceAll(pattern, "");

System.out.println(result); // gibt "12345" aus
```

Hier haben wir beispielsweise alle Nicht-Zahlen-Zeichen aus dem Text entfernt, um nur die Zahl "12345" zu erhalten.

# Tiefergehende Informationen
Die `replaceAll()` Methode nutzt intern die `Pattern` und `Matcher` Klassen aus dem `java.util.regex` Paket. Diese ermöglichen es uns, reguläre Ausdrücke als Muster zu verwenden, um noch genauer zu definieren, welche Zeichen entfernt werden sollen.

Beispielsweise können wir das vorherige Beispiel modifizieren, um auch Kommazahlen zu berücksichtigen.

```Java
String text = "Das ist eine Zahl: 123,45!";
String pattern = "\\D|,";

String result = text.replaceAll(pattern, "");

System.out.println(result); // gibt "12345" aus
```

Hier haben wir sowohl Nicht-Zahlen-Zeichen als auch das Komma aus dem Text entfernt, um nur die Zahl "12345" zu erhalten.

# Siehe auch
- [Oracle Java Dokumentation - Pattern](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Oracle Java Dokumentation - Matcher](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html)
- [W3Schools - Java Regular Expressions](https://www.w3schools.com/java/java_regex.asp)