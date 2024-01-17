---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Java: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was ist das & Warum?
Reguläre Ausdrücke sind spezielle Zeichenfolgen, die verwendet werden, um bestimmte Muster in Texten zu erkennen. Programmierer nutzen sie, um schnell und effizient große Mengen von Text zu durchsuchen und zu manipulieren.

## Wie funktioniert es?
Java bietet standardmäßig eine Klasse namens "Pattern", die es uns ermöglicht, reguläre Ausdrücke zu erstellen und auf Text anzuwenden. Zum Beispiel können wir mit dem folgenden Code eine E-Mail-Adresse aus einem Text extrahieren:

```Java
String text = "Meine E-Mail-Adresse ist max_mustermann@example.com";
Pattern pattern = Pattern.compile("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}");
Matcher matcher = pattern.matcher(text);
if (matcher.find()) {
  System.out.println("Gefundene E-Mail-Adresse: " + matcher.group());
}
```

Ausgabe: Gefundene E-Mail-Adresse: max_mustermann@example.com

## Eintauchen in die Details
Reguläre Ausdrücke haben ihren Ursprung in den 1950er Jahren in der theoretischen Informatik. Heutzutage gibt es viele Alternativen zu regulären Ausdrücken, wie z.B. das "String"-Objekt in Java. Bei der Verwendung von regulären Ausdrücken ist es wichtig zu beachten, dass sie sehr leistungsfähig sind, aber auch sehr komplex. Daher ist es ratsam, sie nur dann zu verwenden, wenn sie tatsächlich benötigt werden.

## Weitere Informationen
- Java Regex Tutorial: https://www.javatpoint.com/java-regex
- Pattern-Klasse in der Java-Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html