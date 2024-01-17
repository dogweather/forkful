---
title:                "Arbeiten mit json"
html_title:           "C#: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Was ist das und warum machen wir es? 
JSON (JavaScript Object Notation) ist ein Format zur Darstellung von strukturierten Daten. Es ist leicht lesbar für Menschen und Maschinen. Programmierer nutzen JSON, um Daten zwischen Anwendungen auszutauschen, da es plattformübergreifend und flexibel ist.

## So geht's: 
Um mit JSON in C# zu arbeiten, müssen Sie zuerst die Newtonsoft.Json Bibliothek installieren. Dann können Sie mit der Deserialisierung und Serialisierung von JSON-Daten beginnen. Schauen wir uns ein Beispiel an:

```C#
var data = "{\"name\":\"Max\", \"age\": 25}";
var person = JsonConvert.DeserializeObject<Person>(data);
Console.WriteLine(person.name); //Output: Max
```
In diesem Beispiel wird eine JSON-Zeichenfolge in ein C# Objekt mit dem Namen "Person" umgewandelt. Mit Hilfe der JsonConvert-Klasse können Daten entweder aus JSON in ein Objekt oder umgekehrt konvertiert werden. 

## Tiefer Einstieg: 
JSON wurde entwickelt, um die Lesbarkeit von Daten für Menschen zu verbessern und verschachtelte Informationen einfach darzustellen. Im Vergleich zu anderen Datenformaten, wie XML, ist JSON weniger komplex und dadurch auch schneller zu verarbeiten. Alternativen zu JSON sind zum Beispiel XML, YAML oder CSV, welche jeweils ihre eigenen Vor- und Nachteile haben. 

Die Implementierung von JSON in C# ist relativ einfach und schnell zu erlernen. Die Newtonsoft.Json Bibliothek bietet viele nützliche Funktionen, um mit JSON-Daten zu arbeiten. Eine ausführliche Dokumentation und eine aktive Community helfen bei der Einarbeitung und Beantwortung von Fragen.

## Weitere Informationen: 
- [Newtonsoft.Json Dokumentation](https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [JSON Wikipedia Eintrag](https://de.wikipedia.org/wiki/JavaScript_Object_Notation)
- [XML vs. JSON vs. YAML vs. CSV Vergleich](https://www.guru99.com/json-v-s-xml.html)