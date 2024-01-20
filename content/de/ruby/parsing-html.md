---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# Parsen von HTML mit Ruby

## Was & Warum?

HTML-Parsing ist der Prozess, bei dem HTML-Code analysiert und strukturiert wird. Programmierer machen das, um auf spezifische Elemente im Code zuzugreifen, Daten zu extrahieren und Web Scraping zu betreiben.

## Wie es geht:

Die Verwendung der Nokogiri Gem in Ruby macht das Parsen von HTML einfach und praktisch. Installieren Sie zunächst Nokogiri mit `gem install nokogiri`.

```ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(open("https://www.example.com"))

# Sucht das erste H1-Tag
h1 = doc.at_css('h1')
puts h1.text
```

Dabei greifen wir auf `https://www.example.com` zu und suchen das erste `<h1>` Tag auf der Seite, dann gibt das Skript dessen Text aus.

## Vertiefung:

HTML-Parsing ist ein altbekanntes Thema in der Web-Entwicklung, mit zahlreichen Lösungsansätzen in unterschiedlichen Sprachen.

Alternativ zu Nokogiri gibt es in Ruby andere Möglichkeiten, HTML zu parsen, wie Hpricot (wird nicht mehr aktiv entwickelt) und Oga. Während Nokogiri auf das libxml2-Framework von Gnu basiert, welches komplizierte Callbacks und potentiell unvorhersehbare Speicherbelegung verursachen kann, behandelt Oga das Parsen auf Ruby-Ebene, was eine bessere Kontrolle ermöglicht.

Details der Implementierung hängen vom spezifischen Fall ab. Eckdaten sind zum Beispiel, ob das HTML wohlgeformt ist, und welches Volumen an Daten gehandhabt werden muss.

## Siehe auch:

- Nokogiri Dokumentation: https://nokogiri.org
- Oga Dokumentation: https://github.com/YorickPeterse/oga
- Hpricot Dokumentation: https://github.com/hpricot/hpricot/wiki
- Mehr Informationen über HTML Parsing: https://www.w3.org/TR/html51/syntax.html#parsing