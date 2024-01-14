---
title:                "Java: Löschen von Zeichen, die einer bestimmten Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einer bestimmten Muster entsprechen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Java-Programmierung nützlich sein, um beispielsweise ungewollte Leerzeichen oder Sonderzeichen aus einer Zeichenkette zu entfernen.

## Wie geht das?

Um Zeichen basierend auf einem bestimmten Muster zu löschen, können wir die replaceAll() Methode der String-Klasse verwenden. Diese Methode akzeptiert als ersten Parameter ein reguläres Ausdruck (Regex) und als zweiten Parameter den Ersatz, der anstelle der gefundenen Zeichen eingesetzt werden soll. Zum Beispiel:

```Java
String text = "Hallo liebe Welt!";
String newText = text.replaceAll("liebe", "");
System.out.println(newText); // Ausgabe: Hallo Welt!
```

In diesem Beispiel verwenden wir die replaceAll() Methode, um das Wort "liebe" aus dem ursprünglichen Text zu entfernen. Beachte, dass der reguläre Ausdruck in Anführungszeichen gesetzt werden muss.

Wenn wir nur bestimmte Zeichen löschen möchten, können wir auch die replace() Methode verwenden, die statt eines regulären Ausdrucks ein einzelnes Zeichen oder eine Zeichenfolge als ersten Parameter akzeptiert.

```Java
String text = "Hallo12345";
String newText = text.replace("12345", "");
System.out.println(newText); // Ausgabe: Hallo
```

Hier wird die Zeichenfolge "12345" durch einen leeren String ersetzt, was dazu führt, dass sie aus dem ursprünglichen Text entfernt wird.

## Tieferer Einblick

Für eine genauere Kontrolle über die zu löschenden Zeichen, können wir auch die replaceAll() Methode zusammen mit einem regulären Ausdruck verwenden, der eine Zeichenklasse enthält. Eine Zeichenklasse ermöglicht es uns, anzugeben, welche Art von Zeichen wir löschen möchten. Zum Beispiel, um alle Sonderzeichen aus dem Text zu entfernen, könnten wir diesen regulären Ausdruck verwenden: "[^a-zA-Z0-9 ]+". Dies bedeutet, dass alles außer Buchstaben und Zahlen sowie Leerzeichen gelöscht werden soll.

```Java
String text = "Dies ist ein Text mit #Sonderzeichen!";
String newText = text.replaceAll("[^a-zA-Z0-9 ]+", "");
System.out.println(newText); // Ausgabe: Dies ist ein Text mit Sonderzeichen
```

Es gibt viele verschiedene Arten von Zeichenklassen, die in regulären Ausdrücken verwendet werden können, um bestimmte Zeichen zu identifizieren. Eine vollständige Liste findest du in der Java-Dokumentation.

## Siehe auch

Java-Dokumentation zum regulären Ausdruck: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html

Tutorial zu regulären Ausdrücken: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html

Stack Overflow Frage zum Löschen von Zeichen mit regulären Ausdrücken: https://stackoverflow.com/questions/4189608/replace-character-in-string-with-java