---
title:                "Arbeiten mit JSON"
html_title:           "C#: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Möglicherweise fragst du dich, warum du dich mit JSON beschäftigen solltest. Nun, JSON ist eine zugängliche und äußerst nützliche Möglichkeit, Daten zu speichern und auszutauschen. Es bietet eine einfache Syntax und wird von vielen Programmiersprachen, einschließlich C#, unterstützt.

## How To

Um mit JSON in C# zu arbeiten, musst du zuerst sicherstellen, dass du die richtigen Tools hast. Die meisten aktuellen Versionen von Visual Studio enthalten bereits eine Bibliothek namens "Newtonsoft.Json", die die Arbeit mit JSON erleichtert.

Hier ist ein Beispiel, wie du Daten in JSON-Format konvertieren kannst:

```C#
using Newtonsoft.Json; //Importiere die notwendige Bibliothek

var myObject = new { Name = "Max", Age = 25 }; //Erstelle ein Objekt mit einigen Eigenschaften
string json = JsonConvert.SerializeObject(myObject); //Konvertiere das Objekt in JSON-Format
Console.WriteLine(json); //Gebe das JSON aus, um die Ausgabe zu sehen
```

Als Ergebnis siehst du folgendes JSON:

```
{"Name":"Max","Age":25}
```

Und hier ist ein Beispiel, wie du JSON-Daten in ein C# Objekt konvertieren kannst:

```C#
string json = @"{ 'Name': 'Anna', 'Age': 30 }"; //Strings in C# müssen in einfachen Anführungszeichen sein
var myObject = JsonConvert.DeserializeObject<Person>(json); //Person ist hier eine Klasse, die die Eigenschaften Name und Age hat
Console.WriteLine(myObject.Name); //Gebe den Namen aus (Ausgabe: Anna)
Console.WriteLine(myObject.Age); //Gebe das Alter aus (Ausgabe: 30)
```

## Deep Dive

JSON (JavaScript Object Notation) ist ein Datenformat, das auf JavaScript basiert, aber von vielen anderen Programmiersprachen verwendet werden kann. Es ist einfach zu lesen und zu schreiben, was es zu einer bevorzugten Wahl für die Datenübertragung und Speicherung macht.

In C# werden JSON-Daten in der Regel als Strings interpretiert und können mithilfe der Klasse "JsonConvert" verarbeitet werden. Diese Klasse enthält Methoden zum Konvertieren von C# Objekten in JSON und umgekehrt. Sie bietet auch Möglichkeiten, das JSON-Format anzupassen, wie zum Beispiel das Ignorieren bestimmter Eigenschaften oder das Hinzufügen von zusätzlichen Eigenschaften.

Eine andere nützliche Funktion von JSON ist die Möglichkeit, mehrere Objekte in einer Datei oder einem String zu speichern. Dies erleichtert die Speicherung und den Austausch von komplexen Datenstrukturen.

## Siehe auch

- [JSON in C# - Entwicklerhandbuch von Microsoft](https://docs.microsoft.com/de-de/dotnet/standard/serialization/system-text-json-how-to)
- [JSON Serializer in C# und .NET](https://www.newtonsoft.com/json)
- [Einführung in JSON - Codecademy Kurs](https://www.codecademy.com/de/courses/javascript-learn-json/lessons/introduction-to-json/exercises/why-json)