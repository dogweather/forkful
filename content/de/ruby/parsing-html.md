---
title:                "HTML parsen"
date:                  2024-01-20T15:33:23.863017-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parsing ist das Einlesen und Analysieren von HTML-Code, um dessen Struktur und Inhalte zu verarbeiten. Programmierer nutzen dies, um Daten aus Webseiten auszulesen oder automatisierte Tests für Webanwendungen durchzuführen.

## Anleitung:
Um HTML in Ruby zu parsen, eignet sich die `nokogiri`-Gems hervorragend. Zunächst musst du die Gem installieren:

```ruby
gem install nokogiri
```

Dann kannst du ein einfaches Script schreiben:

```ruby
require 'nokogiri'
require 'open-uri'

# HTML von einer URL laden
doc = Nokogiri::HTML(URI.open('https://www.beispiel.de'))

# Nach einem HTML-Element suchen
element = doc.at_css('h1')

# Inhalt des Elements ausgeben
puts element.content
```

Ausgabebeispiel, vorausgesetzt `<h1>` ist vorhanden:

```
Willkommen auf meiner Webseite!
```

## Deep Dive:
`Nokogiri` ist japanisch für 'Säge' und symbolisiert, wie die Bibliothek durch HTML- oder XML-Strukturen "schneidet". Sie basiert auf `libxml2` und `libxslt`, bewährt und leistungsstark. Alternativen wie `Oga` oder `Hpricot` (nicht mehr gewartet) können ebenfalls verwendet werden, aber `Nokogiri` ist der Industriestandard. Bei der Implementation von `Nokogiri` ist es wichtig, dass du die Suchmethoden wie `css` für CSS-Selektoren oder `xpath` für XPath-Abfragen nutzt, um spezifische Daten zu extrahieren.

## Siehe Auch:
- [Nokogiri Dokumentation](https://nokogiri.org/)
- [W3Schools CSS Selektoren](https://www.w3schools.com/cssref/css_selectors.asp)
- [XPath und XSLT mit Nokogiri](https://nokogiri.org/tutorials/modifying_an_html_xml_document.html)
