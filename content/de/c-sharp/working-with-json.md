---
title:                "C#: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es immer neue Technologien und Werkzeuge, die man beherrschen muss, um erfolgreich zu sein. Eine davon ist JSON, eine Textformatierung für den Austausch von Daten, die in vielen Anwendungen und APIs verwendet wird. Warum sollte jemand also Zeit und Mühe in das Erlernen und Verwenden von JSON investieren?

JSON ist ein sehr einfaches und leicht verständliches Format, das sowohl von Menschen als auch von Maschinen gelesen und geschrieben werden kann. Es ist auch plattformübergreifend und kompatibel mit vielen Programmiersprachen. Durch den Einsatz von JSON in Ihren Projekten können Sie die Interoperabilität verbessern und die Kommunikation mit anderen Anwendungen erleichtern. Außerdem ist es eine wertvolle Fähigkeit, die immer mehr von Arbeitgebern gesucht wird.

## Wie

Um mit JSON in C# zu arbeiten, müssen Sie zunächst eine JSON-Bibliothek oder ein Framework importieren, das Ihnen das Lesen, Schreiben und Parsen von JSON-Daten ermöglicht. Ein beliebtes Beispiel dafür ist die NuGet-Bibliothek "Newtonsoft.Json".

Nachdem Sie die Bibliothek installiert haben, können Sie mit dem Erstellen von JSON-Objekten beginnen. Hier ist ein Beispiel, wie man ein Objekt mit der Klasse "JObject" erstellen kann:

```C#
JObject obj = new JObject();
obj.Add("Name", "Max Mustermann");
obj.Add("Alter", 30);
```

Dieses Beispiel erstellt ein JSON-Objekt mit den Schlüssel-Wert-Paaren "Name" und "Alter". Nun können Sie dieses Objekt in ein JSON-Format konvertieren und in einer Textdatei speichern oder es an eine API senden.

Um ein JSON-Objekt aus einer Datei zu lesen und zu parsen, können Sie die Methode "JObject.Parse()" verwenden. Ein Beispiel dafür könnte so aussehen:

```C#
string json = File.ReadAllText("Beispiel.json");
JObject obj = JObject.Parse(json);
Console.WriteLine(obj["Name"]);
```

Dieses Beispiel liest ein JSON-Objekt aus der Datei "Beispiel.json" ein und gibt den Wert des Schlüssels "Name" aus.

## Deep Dive

Wenn Sie tiefer in die Arbeit mit JSON einsteigen möchten, gibt es noch viele weitere Konzepte zu entdecken, wie z.B. das Erstellen von Arrays oder das Arbeiten mit verschachtelten JSON-Objekten. Es gibt auch viele Möglichkeiten, wie Sie JSON in Ihren Anwendungen verwenden können, wie z.B. das Abrufen von Daten von einer API oder das Speichern von Benutzereingaben.

Ein wichtiger Punkt beim Umgang mit JSON ist auch die Validierung. Es ist wichtig sicherzustellen, dass die JSON-Daten, die Sie erhalten oder senden, gültig sind. Dafür gibt es viele Online-Tools oder es können Validierungsregeln in Ihren Code eingebaut werden.

Es empfiehlt sich auch, immer die Dokumentation der verwendeten Bibliotheken zu lesen, um alle Funktionen und Möglichkeiten voll auszuschöpfen.

## Siehe auch

- [JSON Tutorial auf Deutsch von freecodecamp](https://www.freecodecamp.org/news/json-deutsch/)
- [Newtonsoft.Json Dokumentation](https://www.newtonsoft.com/json/help/html/R_WhatIsJsonSerialization.htm)
- [Online JSON-Validierer](https://jsonformatter.curiousconcept.com/)