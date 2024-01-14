---
title:                "Java: Verwendung von regulären Ausdrücken"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug in der Java Programmierung, das dir dabei helfen kann, komplexe Textmuster zu erkennen und zu bearbeiten. Mit regulären Ausdrücken kannst du zum Beispiel Email-Adressen, Telefonnummern oder URLs aus einem Text extrahieren oder bestimmte Wörter oder Zeichenfolgen in einem Text ersetzen. Sie können sich als äußerst nützlich erweisen, wenn du mit großen Datenmengen oder der Verarbeitung von Benutzereingaben in deiner Anwendung arbeitest. In diesem Blog-Beitrag lernst du, wie du reguläre Ausdrücke in Java verwendest und wie sie dir bei der Lösung verschiedener Problemstellungen helfen können.

## Wie benutzt man reguläre Ausdrücke in Java

Um reguläre Ausdrücke in Java verwenden zu können, musst du die Klasse ```java.util.regex.Pattern``` und ```java.util.regex.Matcher``` importieren. Dann kannst du mit dem Pattern-Objekt einen regulären Ausdruck erstellen, der eine bestimmte Zeichenfolge beschreibt, nach der gesucht werden soll. Beispielsweise könnte dein regulärer Ausdruck so aussehen:

```Java
Pattern pattern = Pattern.compile("Hello [A-Za-z]+");
```

Dieser reguläre Ausdruck würde nach einer Zeichenfolge suchen, die mit "Hello" beginnt und danach beliebige Buchstaben von A bis Z oder von a bis z enthält. Dann kannst du mit dem Matcher-Objekt dein reguläres Ausdruck auf einen bestimmten Text anwenden, um zu überprüfen, ob es eine Übereinstimmung gibt.

```Java
String text = "Hello John";
Matcher matcher = pattern.matcher(text);

if (matcher.find()) {
  System.out.println("Es gibt eine Übereinstimmung!");
}
```

In diesem Beispiel würde die Ausgabe "Es gibt eine Übereinstimmung!" erscheinen, da der Text "Hello John" den regulären Ausdruck erfüllt. Zusätzlich kannst du mit regulären Ausdrücken auch Teile des Textes extrahieren, indem du in deinem regulären Ausdruck Gruppierungen mit runden Klammern verwendest. Diese werden dann in der Reihenfolge, in der sie im regulären Ausdruck aufgeführt sind, als Matches zurückgegeben.

```Java
Pattern pattern = Pattern.compile("Hello ([a-zA-Z]+)");
String text = "Hello John";
Matcher matcher = pattern.matcher(text);

if (matcher.find()) {
  String name = matcher.group(1);
  System.out.println("Hallo " + name + "!");
}
```

In diesem Fall würde die Ausgabe "Hallo John!" erscheinen, da der Name "John" in der ersten Gruppierung des regulären Ausdrucks steht.

## Tiefergehende Informationen

Reguläre Ausdrücke können sehr komplex werden und umfassen viele verschiedene Operatoren und Syntaxregeln. Um alle Möglichkeiten von regulären Ausdrücken in Java zu verstehen, empfehle ich dir, die offizielle Java-Dokumentation zur ```java.util.regex``` Klasse zu lesen. Darin findest du ausführliche Erklärungen zu allen relevanten Klassen und Methoden und kannst deine Kenntnisse weiter vertiefen.

## Siehe auch

- [Java-Dokumentation zur ```java.util.regex``` Klasse](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Tutorial zu regulären Ausdrücken in Java von Javatpoint](https://www.javatpoint.com/java-regex)
- [Online RegEx Tester zum Ausprobieren von regulären Ausdrücken](https://regex101.com/)