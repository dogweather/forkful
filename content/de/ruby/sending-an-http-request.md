---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anforderung bedeutet, Daten von einem Server anzufordern oder zu senden. Programmierer tun dies, um Daten mit Webanwendungen auszutauschen oder um RESTful-Webdienste zu nutzen.

## So geht's: 

Hier ist ein einfaches Beispiel dafür, wie man eine GET-Anforderung an eine Web-API sendet und die Antwort ausdruckt. 

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("https://api.example.com/users/123")
response = Net::HTTP.get_response(uri)

puts response.body
```

Bei diesem Beispiel wird eine Anforderung an die URL `https://api.example.com/users/123` gesendet und die Antwort wird auf der Konsole ausgedruckt.

## Tiefgreifende Analyse

Historisch gesehen wurde die Fähigkeit, HTTP-Anforderungen zu senden und zu empfangen, zu den grundlegenden Funktionen des Internets und führte zur Entwicklung des World Wide Web. Das Protokoll hat sich seitdem weiterentwickelt und ist heute ein zentraler Bestandteil der meisten Webanwendungen.

Es gibt verschiedene Bibliotheken und Werkzeuge, die Sie zum Senden von HTTP-Anforderungen in Ruby verwenden können. Neben dem 'net/http'-Modul, das in der Ruby-Standardbibliothek enthalten ist, sind beliebte Alternativen 'httparty' und 'rest-client'. 

Die genauen technischen Details des Sendens einer HTTP-Anfrage hängen von vielen Faktoren ab, einschließlich der verwendeten Bibliothek, der Art der Anforderung (GET, POST, usw.), und der spezifischen Anforderungen der API, mit der Sie kommunizieren.

## Siehe auch:

- httparty Gem: [https://rubygems.org/gems/httparty](https://rubygems.org/gems/httparty)
- RestClient Gem: [https://rubygems.org/gems/rest-client](https://rubygems.org/gems/rest-client)