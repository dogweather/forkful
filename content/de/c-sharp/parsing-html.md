---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

# Zerlegen von HTML in C#

## Was & Warum?

HTML-Zerlegen ist der Prozess der Aufteilung eines HTML-Dokuments in seine Bestandteile mit speziellen Parsing-Techniken. Programmierer machen das, um auf bestimmte Daten in der HTML-Struktur zugreifen oder Manipulationen daran durchführen zu können.

## Wie macht man das?

```C#
// Wir verwenden die HtmlAgilityPack Bibliothek
using HtmlAgilityPack;

HtmlDocument doc = new HtmlDocument();
doc.Load("dateiname.html"); 

// Zugriff auf den Titel (<title> Element) des Dokuments
var title= doc.DocumentNode.SelectSingleNode("//title").InnerText;
Console.WriteLine(title);

// Ausgabe => "Dein Titel"
```
Mit dem obigen Code greifen wir auf das `<title>` Element eines HTML-Dokuments zu und geben dessen Inhalt aus.

## Tiefer Einblick

Historisch gesehen war HTML-Parsing ein herausfordernder Prozess aufgrund verschiedener Browser-Implementierungen und Konformitätsstandards. Heutzutage, dank Bibliotheken wie HtmlAgilityPack, ist das Zerlegen von HTML in C# einfacher und weniger fehleranfällig.

Alternativ können Sie auch Bibliotheken wie AngleSharp verwenden. Es folgt dem W3C-Standard und unterstützt auch CSS-Selektion.

Die im Beispiel verwendete Methode `SelectSingleNode` ist ein fundamentaler Teil des HTML-Dokumentenmodells des HtmlAgilityPack. Es wendet XPATH-Ausdrücke auf das Dokument an, um die Navigation und Selektion von Knoten zu ermöglichen.

## Siehe auch

- Die Dokumentation von HtmlAgilityPack ist eine großartige Ressource, um tiefer in das HTML-Zerlegen einzutauchen. [(Link)](https://html-agility-pack.net/)

Mit den angegebenen Informationen sollten Sie in der Lage sein, den Prozess des Zerlegens von HTML in C# zu verstehen und ihn effektiv in Ihren Projekten zu nutzen.