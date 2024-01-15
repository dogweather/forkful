---
title:                "Senden einer HTTP-Anfrage mit grundlegenden Authentifizierung"
html_title:           "Ruby: Senden einer HTTP-Anfrage mit grundlegenden Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit grundlegenden Authentifizierung"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

HTTP-Anfragen mit Basisauthentifizierung werden oft verwendet, um eine sichere Verbindung zwischen einer Anwendung und einem Server herzustellen. Dies ist besonders nützlich, wenn vertrauliche Daten übertragen werden müssen.

## Wie geht das?

Um eine HTTP-Anfrage mit Basisauthentifizierung in Ruby zu senden, können wir die Net::HTTP-Bibliothek verwenden. Zuerst müssen wir ein URI-Objekt erstellen, das die URL enthält, zu der wir eine Anfrage senden möchten. Dann können wir ein Net::HTTP-Objekt erstellen und die Methode #basic_auth verwenden, um Benutzername und Passwort anzugeben. Schließlich rufen wir die Methode #request auf und übergeben die gewünschte Aktion (GET, POST, PUT, etc.) sowie das URI-Objekt. Hier ist ein Beispielcode:

```Ruby
require 'net/http'

uri = URI('https://www.example.com')
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true

request = Net::HTTP::Get.new(uri)
request.basic_auth("username", "password")

response = http.request(request)

puts response.code
puts response.body
```

Das obige Beispiel stellt eine sichere Verbindung zu www.example.com her, führt eine GET-Anfrage mit Basisauthentifizierung aus und gibt den Statuscode sowie die Antwortdaten aus. Die Methode #basic_auth kann auch auf andere HTTP-Anfragemethoden wie POST und PUT angewendet werden.

## Tiefer Einblick

HTTP-Anfragen mit Basisauthentifizierung verwenden den Authentifizierungstyp "Basic", bei dem der Benutzername und das Passwort in Base64-Format kodiert und als Teil des Authorization-Headers in der Anfrage gesendet werden. Der Server überprüft dann die Gültigkeit der Anmeldedaten und gibt eine entsprechende Antwort zurück. Es ist wichtig zu beachten, dass dieser Vorgang nicht als sicherer Authentifizierungsmechanismus gilt und zusätzliche Sicherheitsmaßnahmen wie SSL/TLS empfohlen werden.

## Siehe auch

- [Net::HTTP Dokumentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [Base64-Bibliothek in Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/base64/rdoc/Base64.html)
- [HTTP-Basisauthentifizierung auf Wikipedia](https://de.wikipedia.org/wiki/HTTP#Authentifizierung)