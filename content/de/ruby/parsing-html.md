---
title:                "Ruby: Das Parsen von HTML"
simple_title:         "Das Parsen von HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Aufbereitung von HTML beschäftigen? Nun, HTML ist die Sprache des Internets und fast jede Webseite, die wir besuchen, basiert auf HTML. Durch das Parsen von HTML können wir also wichtige Informationen aus Webseiten extrahieren und weiterverarbeiten.

## Wie geht das?

Um HTML mit Ruby zu parsen, verwenden wir die Bibliothek Nokogiri. Stellen wir uns vor, wir möchten die Überschriften einer Webseite auslesen. Hier ist ein Beispielcode, wie das mit Nokogiri in Ruby aussehen würde: 

```ruby
require 'nokogiri'
require 'open-uri'
url = "https://example.com"
page = Nokogiri::HTML(open(url))
headings = page.css('h1, h2, h3').text
puts headings
```

Der Code importiert die Nokogiri Bibliothek und öffnet dann die gewünschte URL. Mit `Nokogiri::HTML(open(url))` wandeln wir den HTML-Code der Webseite in ein strukturiertes Dokument um, das wir dann weiterverarbeiten können. In diesem Fall verwenden wir `page.css` um nach den verschiedenen Überschriften zu suchen und dann mit `.text` den reinen Text auszulesen. Diesen geben wir abschließend mit `puts` aus.

Die Ausgabe würde dann folgendermaßen aussehen:

```ruby
"Meine Webseite - Beispieltext
Überschrift 1
Überschrift 2"
```

## Tiefer einsteigen

Natürlich gibt es noch viel mehr Möglichkeiten und Funktionen, wenn es darum geht, HTML mit Ruby zu parsen. Zum Beispiel können wir gezielt nach bestimmten Klassen oder IDs suchen, um spezifische Elemente auszulesen. Hier ist ein Beispielcode, wie das aussehen könnte:

```ruby
require 'nokogiri'
require 'open-uri'
url = "https://example.com"
page = Nokogiri::HTML(open(url))
links = page.css('.link-list li a').map {|link| link['href']}
puts links
```

Dieser Code sucht nach allen Links innerhalb einer Liste mit der Klasse "link-list". Mit `.map` erzeugen wir dann ein Array mit allen gefundenen Links. Die Ausgabe könnte dann in etwa so aussehen:

```ruby
["https://beispiel-link1.com", "https://beispiel-link2.com", "https://beispiel-link3.com"]
```

Es gibt noch viele weitere Möglichkeiten und Funktionen, die man beim Parsen von HTML mit Ruby nutzen kann. Wichtig ist, dass man sich mit der Nokogiri Bibliothek und ihren Funktionen vertraut macht, um das Beste aus dem HTML herauszuholen.

## Siehe auch

1. [Nokogiri Dokumentation](https://nokogiri.org/)
2. [Scrapping mit Ruby](https://www.rubyguides.com/2018/02/parsing-html-ruby-nokogiri/)
3. [Ruby Dokumentation](https://www.ruby-lang.org/de/documentation/)