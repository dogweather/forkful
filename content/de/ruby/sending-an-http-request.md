---
title:                "Eine http-Anfrage senden"
html_title:           "Ruby: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit dem Senden von HTTP-Requests beschäftigen? Nun, viele moderne Anwendungen basieren auf der Kommunikation über das Internet und das Senden von HTTP-Requests ist ein grundlegender Bestandteil davon. Es ermöglicht die Interaktion mit verschiedenen APIs, das Abrufen von Daten von externen Quellen und vieles mehr.

## How To
Das Senden von HTTP-Requests mag zunächst einschüchternd wirken, aber es ist eigentlich recht einfach. Hier ist ein Beispiel mit Ruby:

```Ruby
require 'net/http'

url = URI("https://example.com/api/user")
# Ersetze URL mit der gewünschten Endpunkt-URL

request = Net::HTTP.get(url)
# Ersetze `get` mit der gewünschten HTTP-Methode, wie z.B. `post`, `put`, `delete`

response = JSON.parse(request.body)
# Wenn du mit Daten im JSON-Format arbeitest, kannst du sie mit `JSON.parse` in eine verwertbare Form bringen

puts response
# Gibt die Antwort des Servers aus, abhängig von der gewählten HTTP-Methode
```

Das ist nur ein einfaches Beispiel, aber es zeigt dir, wie du mit Ruby einen HTTP-Request senden und die Antwort verarbeiten kannst.

## Deep Dive
Wenn du noch tiefer in das Thema einsteigen möchtest, solltest du dich mit den verschiedenen HTTP-Methoden auseinandersetzen, wie z.B. `get`, `post`, `put`, `patch` und `delete`. Jede dieser Methoden hat eine spezifische Funktion und kann dir bei der Arbeit mit verschiedenen APIs helfen. Außerdem gibt es noch viele weitere Konzepte, die du lernen kannst, wie z.B. die Verwendung von HTTP-Headern, das Arbeiten mit Authentifizierung und das Behandeln von Fehlern.

## Siehe auch
- [Net::HTTP Dokumentation](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/index.html)
- [Ruby on Rails Guides zu HTTP-Requests](https://guides.rubyonrails.org/action_controller_overview.html#http-methods)
- [RFC 7231 - HTTP/1.1 Methoden](https://tools.ietf.org/html/rfc7231#section-4)