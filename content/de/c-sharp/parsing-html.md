---
title:                "C#: Das Parsen von HTML"
simple_title:         "Das Parsen von HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Warum
Das Parsen von HTML-Dokumenten ist eine essentielle Fähigkeit für jeden C# Programmierer. Es ermöglicht uns, die Inhalte einer Webseite zu extrahieren und sie in unsere Anwendungen zu integrieren. Das kann uns Zeit und Arbeit ersparen und gleichzeitig die Benutzererfahrung verbessern.

## Wie 
Das Parsen von HTML in C# ist relativ einfach und kann mit Hilfe der HtmlAgilityPack Bibliothek durchgeführt werden. Diese Bibliothek ermöglicht es uns, HTML-Dokumente einzulesen und in Form von C# Objekten zu speichern, die wir dann weiterverarbeiten können.

Hier ist ein Beispiel, wie wir den Titel und den Inhalt einer Webseite mithilfe des HtmlAgilityPack auslesen können:

```C#
// Webseite einlesen
var doc = new HtmlAgilityPack.HtmlDocument();
doc.Load("https://www.example.com");

// Den Titel auslesen
var title = doc.DocumentNode.SelectSingleNode("//title").InnerText;
Console.WriteLine($"Titel: {title}");

// Den Inhalt auslesen
var content = doc.DocumentNode.SelectSingleNode("//div[@class='content']").InnerText;
Console.WriteLine($"Inhalt: {content}");
```

Die Ausgabe sieht dann folgendermaßen aus:

```
Titel: Beispiel Webseite
Inhalt: Willkommen auf unserer Webseite! Hier finden Sie nützliche Informationen und Tipps.
```

## Deep Dive
Beim Parsen von HTML geht es nicht nur darum, bestimmte Informationen aus einer Webseite auszulesen, sondern auch darum, verschiedene HTML Elemente wie Tags, Attribute und deren Hierarchie zu verstehen.

Der HtmlAgilityPack bietet zahlreiche Funktionen, um auf diese Elemente zuzugreifen und sie zu manipulieren. Man kann zum Beispiel bestimmte Elemente anhand ihres Tags, Attributes oder Inhalts auswählen und ändern. Auch das Finden von spezifischen Teilbäumen oder das Validieren von HTML-Dokumenten ist möglich.

Es ist wichtig zu beachten, dass das Parsen von HTML nicht immer perfekt sein kann, da Webseiten oft unvorhersehbar gestaltet sind und sich auch im Laufe der Zeit ändern können. Es kann daher nötig sein, unsere Parser anzupassen, um fehlerhafte oder unerwartete Ergebnisse zu vermeiden.

## Siehe auch
- [HtmlAgilityPack Dokumentation](https://html-agility-pack.net/)
- [Offizielles C# 9.0 Handbuch](https://docs.microsoft.com/de-de/dotnet/csharp/)
- [Interessante Projekte auf GitHub](https://github.com/search?q=C%23+html+parser)