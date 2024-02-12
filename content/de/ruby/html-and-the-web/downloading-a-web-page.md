---
title:                "Webseite herunterladen"
aliases:
- /de/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:44:48.997964-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, ihren Inhalt über das Internet abzurufen. Programmierer tun dies, um Daten zu sammeln, automatisiert Inhalte zu prüfen oder Webservices zu nutzen.

## How-to:
Zum Herunterladen einer Webseite in Ruby nutzen wir die Bibliothek ‘net/http’. Hier ist ein einfaches Beispiel:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get_response(uri)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Ergebnis:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive:
Früher waren Optionen zum Herunterladen von Webinhalten in Ruby begrenzt. Mit der Zeit wurden spezialisierte Bibliotheken entwickelt. ‘Open-uri’ ist eine solche Bibliothek, die einfach zu nutzen ist, aber ‘net/http’ bietet mehr Kontrolle. Überlegungen zur Implementierung umfassen das Handling von Weiterleitungen, HTTPS und Cookies. Asynchrone HTTP-Bibliotheken wie ‘httparty’ oder ‘faraday’ bieten alternative Ansätze mit mehr Funktionen.

## See Also:
- Ruby-Dokumentation für `net/http`: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- Ruby-Dokumentation für `uri`: https://ruby-doc.org/stdlib-3.0.0/libdoc/uri/rdoc/URI.html
- `httparty` GitHub: https://github.com/jnunemaker/httparty
- `faraday` GitHub: https://github.com/lostisland/faraday
