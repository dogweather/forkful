---
title:                "HTML verarbeiten"
html_title:           "Ruby: HTML verarbeiten"
simple_title:         "HTML verarbeiten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen von HTML ist der Prozess, bei dem ein Computer Daten aus HTML-Code extrahiert. Programmierer verwenden es, um automatisiert Daten aus Webseiten zu sammeln und zu analysieren.

## Wie geht's?

Die Ruby-Bibliothek 'Nokogiri' ist ein leistungsstarkes Werkzeug zum Parsen von HTML. Hier ist ein einfaches Beispiel, das den Titel einer Webseite ausgibt:

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(open('https://www.google.com'))
puts doc.title
```

Dieses Beispiel verwendet die 'open-uri' Bibliothek, um den HTML-Code von der Webseite zu holen, und dann verwendet Nokogiri, um den Titel aus dem HTML-Code zu extrahieren.

## Tiefer Tauchen

Das Parsen von HTML hat eine lange Geschichte, die bis in die Anfänge des Webs zurückgeht. Frühere Methoden, wie zum Beispiel reguläre Ausdrücke, waren nicht so robust wie heutige Tools wie Nokogiri.

Alternativen zu Nokogiri sind unter anderem die Bibliotheken 'Hpricot' und 'Mechanize'. Diese bieten ähnliche Funktionen, sind aber möglicherweise nicht so performant wie Nokogiri.

Die Implementierung von Nokogiri basiert auf der 'libxml2' Bibliothek, die in C geschrieben ist. Dies trägt zur hohen Geschwindigkeit von Nokogiri bei.

## Siehe auch

Weitere Informationen zu Nokogiri und dem Parsen von HTML mit Ruby finden Sie auf der offiziellen Dokumentationsseite: https://nokogiri.org/

Eine ausführliche Anleitung zum Parsen von HTML mit 'Nokogiri' finden Sie hier: https://www.rubyguides.com/2018/10/parsing-html-in-ruby/

Weitere Alternativen und Vergleiche von HTML-Parsing-Tools finden Sie hier: https://rubygems.org/gems/nokogiri/versions/1.6.2.1.