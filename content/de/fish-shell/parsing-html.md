---
title:                "Fish Shell: Html analysieren"
simple_title:         "Html analysieren"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

HTML ist eine der meistgenutzten Auszeichnungssprachen im Web und steht für Hypertext Markup Language. Es wird verwendet, um Webseiten zu erstellen und zu gestalten. Das Parsen von HTML ist somit ein wichtiger Schritt beim Entwickeln von Webanwendungen oder beim Web-Scraping.

## Wie man HTML in der Fish Shell parst

Um HTML in der Fish Shell zu parsen, benötigt man ein geeignetes Tool. In diesem Beispiel verwenden wir die fish-htmlparse-Bibliothek, die mit Hilfe von xpath HTML-Dokumente durchsuchen kann.

Zunächst müssen wir die Bibliothek installieren, entweder über den Paketmanager unserer Wahl oder direkt von der Fish-Shell aus:

```Fish Shell
apt-get install fish-htmlparse
```

Als nächstes importieren wir die Bibliothek und geben die URL eines HTML-Dokuments an, das wir parsen möchten:

```Fish Shell
source (fish-htmlparse)
set html (http --body https://www.example.com)
```

Jetzt können wir xpath verwenden, um bestimmte Elemente in dem HTML-Dokument zu finden und deren Inhalt auszugeben:

```Fish Shell
xpath $html '//h1'
```

Dieser Befehl gibt alle h1-Elemente auf der Seite aus. Man kann auch spezifische Attribute der Elemente ausgeben, z.B. die hrefs der Links:

```Fish Shell
xpath -t '@href' $html '//a'
```

## Tieferer Einblick in das Parsen von HTML

Xpath ist eine mächtige Abfragesprache, die beim Durchsuchen von XML- oder HTML-Dokumenten verwendet werden kann. Man kann damit gezielt nach bestimmten Elementen, Attributen oder Texten suchen und so Daten aus dem Dokument extrahieren. Es gibt auch andere Möglichkeiten, HTML in der Fish Shell zu parsen, wie zum Beispiel die Verwendung von regulären Ausdrücken oder der Nutzung von Bibliotheken wie HTML-Parser.

Das Parsen von HTML kann auch herausfordernd sein, da die Struktur des Dokuments variieren kann und es möglicherweise nicht gültiges HTML enthält. Es erfordert daher eine sorgfältige Planung und Anpassung des Parsings auf die spezifische Aufgabe.

## Siehe auch

- [Fish Shell-Offizielle Dokumentation](https://fishshell.com/docs/current/)
- [Xpath-Tutorial](https://www.w3schools.com/xml/xpath_intro.asp)
- [HTML-Parser-Bibliothek](https://htmlparser.sourceforge.io/)