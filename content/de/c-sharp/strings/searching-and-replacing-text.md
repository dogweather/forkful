---
date: 2024-01-20 17:57:37.581623-07:00
description: "So geht\u2019s: Ein einfaches Beispiel in C# zeigt, wie man den `string.Replace`-Methode\
  \ zum Ersetzen von Text verwendet."
lastmod: '2024-03-13T22:44:53.873423-06:00'
model: gpt-4-1106-preview
summary: Ein einfaches Beispiel in C# zeigt, wie man den `string.Replace`-Methode
  zum Ersetzen von Text verwendet.
title: Suchen und Ersetzen von Text
weight: 10
---

## So geht’s:
Ein einfaches Beispiel in C# zeigt, wie man den `string.Replace`-Methode zum Ersetzen von Text verwendet:

```C#
string originalText = "Hallo Welt! C# macht Spaß.";
string modifiedText = originalText.Replace("Spaß", "Freude");

Console.WriteLine(modifiedText);
```

Ausgabe:

```
Hallo Welt! C# macht Freude.
```

Für komplexere Fälle mit Mustern benutzt man reguläre Ausdrücke (`Regex`):

```C#
using System.Text.RegularExpressions;

string originalText = "Die Postleitzahl 12345 könnte auch 67890 sein.";
string pattern = @"\b\d{5}\b";
string replacement = "#####";

string resultText = Regex.Replace(originalText, pattern, replacement);

Console.WriteLine(resultText);
```

Ausgabe:

```
Die Postleitzahl ##### könnte auch ##### sein.
```

## Deep Dive
Textsuche und -ersatz sind in der Softwareentwicklung unverzichtbare Werkzeuge seit den frühen Tagen von Editoren und Programmiersprachen. In C# macht die Klasse `String` diesen Vorgang mit Methoden wie `Replace` sehr zugänglich. Für mehr Komplexität bietet `System.Text.RegularExpressions.Regex` Mustererkennung und Ersatzoperationen. 

Alternativen zu `string.Replace` sind StringBuilder-Methoden, wenn es um Leistung bei vielen Änderungen geht. In LINQ kann man mit `Aggregate` ähnliche Resultate erzielen, aber weniger effizient.

Details zur Implementierung von `Replace`: Es erstellt einen neuen String, weil Strings in C# unveränderlich (immutable) sind. `Regex` hingegen arbeitet mit kompilierten Mustern und kann daher schneller sein bei häufigem Einsatz.

## Siehe auch:
- Microsoft Dokumentation zur `String`-Klasse: [docs.microsoft.com/en-us/dotnet/api/system.string](https://docs.microsoft.com/en-us/dotnet/api/system.string)
- Microsoft Dokumentation zu `Regex`: [docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
- Einführung in reguläre Ausdrücke: [regular-expressions.info](https://www.regular-expressions.info/)
