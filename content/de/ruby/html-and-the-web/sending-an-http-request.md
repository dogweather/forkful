---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T18:01:25.063817-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP-Anfrage ist ein zentraler Bestandteil des Web, bei dem Daten von oder zu einem Server übertragen werden. Programmierer nutzen das, um mit Webdiensten zu interagieren, Ressourcen abzurufen oder Daten zu übermitteln.

## How to:
Verwende die 'net/http' Standardbibliothek von Ruby, um eine einfache GET-Anfrage zu senden:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get_response(uri)
puts response.body
```

Sample output für eine erfolgreiche Antwort könnte so aussehen:

```Ruby
<!doctype html>
<html>
...
</html>
```

Um eine POST-Anfrage zu senden, baust du es ein wenig um:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
request = Net::HTTP::Post.new(uri, 'Content-Type' => 'application/json')
request.body = '{ "my_data": "value" }'
response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

puts response.body
```

## Deep Dive
Die Fähigkeit, HTTP-Anfragen zu senden, entstand früh in der Entwicklung des Internets, als es darum ging, wie Computer miteinander sprechen. Es gibt auch andere Bibliotheken in Ruby, wie `Faraday` oder `HTTParty`, die den gleichen Zweck erfüllen, oft mit mehr Funktionen oder einfacherer Syntax. Bei der Implementierung von HTTP-Anfragen musst du auch Aspekte wie Timeouts, Error Handling und Header Management im Blick haben. Die Wahl der Bibliothek kann von diesen Anforderungen stark beeinflusst werden.

## See Also
- HTTParty GitHub repository: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- Faraday GitHub repository: [https://github.com/lostisland/faraday](https://github.com/lostisland/faraday)
