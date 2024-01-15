---
title:                "Herunterladen einer Webseite"
html_title:           "Ruby: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon immer mal wissen wolltest, wie man eine Webseite herunterladen kann, bist du hier genau richtig! Mit Ruby ist es ganz einfach, eine Webseite herunterzuladen und den HTML-Code zu analysieren.

## Wie funktioniert es?

Um eine Webseite herunterzuladen, verwenden wir die Ruby Standard Library `net/http`. Zuerst müssen wir die URL der Webseite angeben, die wir herunterladen möchten.

```Ruby
require 'net/http'
url = URI.parse('https://www.ruby-lang.org/de/')
```

Dann verwenden wir `Net::HTTP.get_response` um die Antwort von der Webseite zu erhalten.

```Ruby
response = Net::HTTP.get_response(url)
```

Der zurückgegebene Wert ist ein Objekt der Klasse `Net::HTTPResponse`, welches Informationen wie den Statuscode und den HTML-Inhalt der Seite enthält. Wir können `body` verwenden, um den HTML-Code der Seite zu erhalten.

```Ruby
puts response.body
```

Dies gibt uns den gesamten HTML-Code der Webseite aus. Wir können auch zusätzliche Informationen wie den Response Code und die Header der Seite anzeigen lassen.

```Ruby
puts response.code # Gibt den Statuscode der Antwort aus (z.B. 200 für Erfolg)
puts response.header # Gibt die Header der Antwort aus (z.B. Content-Type)
```

## Tief tauchen

Wenn wir uns den HTML-Code genauer ansehen möchten, können wir Tools wie Nokogiri verwenden, um den Code zu analysieren und spezifische Elemente auszuwählen.

```Ruby
require 'nokogiri'
doc = Nokogiri::HTML(response.body)

# Gibt den Titel der Seite aus
puts doc.css('title').text

# Gibt alle Links auf der Seite aus
puts doc.css('a').to_s
```

Nokogiri hat viele mächtige Funktionen, die es uns ermöglichen, auf spezifische Elemente im HTML-Code zuzugreifen und sie zu manipulieren.

## Siehe auch

- [Net::HTTP Dokumentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [Nokogiri Dokumentation](https://nokogiri.org/)