---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, Daten von einem Server im Internet zu bekommen. Programmierer tun dies, um Informationen zu sammeln, Datenanalysen durchzuführen oder Inhalte offline zu speichern.

## Wie geht das?

Um eine Webseite in Ruby herunterzuladen, können wir die `net/http` Bibliothek benutzen. Hier ist ein einfacher Ruby-Code zum Herunterladen einer Webseite:

```Ruby
require 'net/http'
require 'uri'

def download_webpage(url)
  uri = URI.parse(url)
  response = Net::HTTP.get_response(uri)

  if response.code == "200"
    puts response.body
  else
    puts "Fehler beim Herunterladen der Webseite"
  end
end

download_webpage("http://example.com")
```

Wenn du das ausführst, siehst du den HTML-Code der Webseite.
 
## Tiefere Einblicke

Zum Herunterladen von Webseiten wurde Ruby erstmalig im Jahr 1995 eingesetzt. Heutzutage gibt es jedoch andere Werkzeuge wie `HTTParty` und `curb`, die häufiger verwendet werden, weil sie leistungsfähiger und flexibler sind. Bei der Umsetzung werden HTTP-GET-Anfragen gesendet, um Webseiteninhalte abzurufen. Beachte jedoch, dass manche Seiten gegen das Crawlen geschützt sind und du deren Robots.txt-Regeln einhalten solltest.

## Siehe auch

- [Net::HTTP Dokumentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTParty Gem](https://rubygems.org/gems/httparty)
- [Curb Gem](https://rubygems.org/gems/curb)