---
title:    "Java: Verwendung von regulären Ausdrücken"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug in der Java-Programmierung, um Muster in Strings zu erkennen und manipulieren. Sie ermöglichen es, komplexe Suchen und Ersetzungen durchzuführen, ohne eine große Anzahl von Schleifen und Bedingungen schreiben zu müssen.

## Wie man reguläre Ausdrücke verwendet

Um reguläre Ausdrücke in Java zu verwenden, müssen wir die Klasse `java.util.regex.Pattern` importieren. Dann können wir eine Instanz von `Pattern` erstellen, indem wir einen regulären Ausdruck als String übergeben.

Ein Beispiel:

```Java
import java.util.regex.Pattern;

String regex = "a*b"; // sucht nach dem Muster "a" gefolgt von 0 oder mehr "b"
Pattern pattern = Pattern.compile(regex);
```

Wir können nun die Pattern-Instanz verwenden, um in Strings nach Mustern zu suchen oder diese zu ersetzen. Zum Beispiel können wir mit der Methode `matcher()` auf einem String eine Instanz von `Matcher` erstellen, mit der wir dann verschiedene Operationen durchführen können.

```Java
Matcher matcher = pattern.matcher("abbabababb");
boolean match = matcher.matches(); // gibt true aus, weil der String dem Muster entspricht
```

Eine vollständige Liste der Methoden für Pattern und Matcher finden Sie in der offiziellen Java-Dokumentation.

## Tiefer einsteigen

Reguläre Ausdrücke können noch viel mehr als nur einfache Mustererkennung. Sie können auch Variablen und Quantoren verwenden, um komplexe Muster zu definieren. Einige wichtige Begriffe zu beachten sind: Metazeichen, reguläre Ausdruckssyntax und Gruppierung.

Ein Beispiel für ein fortgeschritteneres Muster:

```Java
String regex = "(\\d{3}[.-]){2}\\d{4}"; // sucht nach einer Telefonnummer im Format 123-456-7890
```

Es gibt auch zahlreiche Online-Tools und Bibliotheken, die Ihnen beim Erstellen und Testen von regulären Ausdrücken helfen können.

## Siehe auch

- [Java-Dokumentation zu regulären Ausdrücken](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [RegexTester – online reguläre Ausdrücke testen](https://regex101.com/)
- [RegExLib – Sammlung von regulären Ausdrücken für verschiedene Zwecke](https://www.regexplib.com/)