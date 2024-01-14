---
title:    "Java: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in vielen Situationen nützlich sein, wie zum Beispiel beim Bereinigen von Daten oder beim Filtern von Eingaben in einer Anwendung. In diesem Blog-Beitrag zeigen wir Ihnen, wie Sie dies in Java programmieren können.

## Wie geht man vor

Die grundlegendste Methode zum Löschen von Zeichen gemäß einem Muster ist die Verwendung der `replaceAll()`-Funktion aus der `String`-Klasse. Hier ist ein Beispielcode, der alle Vokale aus einem String entfernt:

```Java
String text = "Hallo Welt!";
text = text.replaceAll("[aeiou]", "");
System.out.println(text); // Ddr!
```

Ein Assistent für reguläre Ausdrücke kann Ihnen dabei helfen, das richtige Muster zu finden. Außerdem können Sie die `Pattern`- und `Matcher`-Klassen verwenden, um komplexere Muster zu definieren und anzuwenden. Hier ist ein Beispiel, das alle Satzzeichen aus einem String entfernt:

```Java
String text = "Hört auf, mich zu stören!";
Pattern pattern = Pattern.compile("[.,:;?!]");
Matcher matcher = pattern.matcher(text);
text = matcher.replaceAll("");
System.out.println(text); // Hört auf mich zu stören
```

Es ist wichtig zu beachten, dass die `replaceAll()`-Funktion ein neues `String`-Objekt zurückgibt und das ursprüngliche Objekt unverändert lässt. Wenn Sie das ursprüngliche Objekt ändern möchten, müssen Sie die `replaceFirst()`- oder die `replace()`-Funktion verwenden.

## Tiefer Einblick

Reguläre Ausdrücke bieten eine leistungsstarke und flexible Möglichkeit, nach Mustern in Zeichenketten zu suchen und diese zu ändern. Man kann auch Gruppierungen in regulären Ausdrücken verwenden, um Teile eines Strings zu extrahieren oder mit diesen zu arbeiten. Hier ist ein Beispiel, das alle Wörter in einem String ausgibt, die mit "hi" beginnen:

```Java
String text = "Hi, ich bin ein Hi-Tech-Entwickler!";
Pattern pattern = Pattern.compile("\\bhi\\w*");
Matcher matcher = pattern.matcher(text);
while (matcher.find()) {
    System.out.println(matcher.group()); // Hi, Hi-Tech
}
```

Es gibt viele verschiedene Zeichenklassen, Quantifikatoren und andere Funktionen, die in regulären Ausdrücken verwendet werden können. Wir empfehlen Ihnen, sich über diese Funktionalitäten weiter zu informieren, um die volle Flexibilität und Power von regulären Ausdrücken in Java zu nutzen.

## Siehe auch

- [Java SE 11 Dokumentation zu regulären Ausdrücken](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/Pattern.html)
- [Tutorial zu regulären Ausdrücken in Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Online-RegEx-Tester für Java](https://www.freeformatter.com/java-regex-tester.html)