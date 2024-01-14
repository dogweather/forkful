---
title:                "Ruby: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTPS-begäran med grundläggande autentisering är en vanlig metod inom Ruby-programmering för att autentisera användare och skicka information till en webbsida eller server. Det används också för att göra API-anrop till fjärranslutna resurser.

## Hur man gör
För att skicka en HTTP-begäran med grundläggande autentisering i Ruby, behöver du först en URL till webbadressen som du vill skicka begäran till. Detta kan se ut som följande i kod:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("https://www.example.com/api")
```

Nästa steg är att skapa en instans av Net::HTTP-klassen och skicka en GET-begäran med lämplig HTTP-förfrågan och autentisering i HTTP-begäran. Detta kan göras på följande sätt:

```Ruby
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth("användarnamn", "lösenord")
response = http.request(request)
```

Därefter kan du läsa av svaret från servern genom att använda "response.body" som kommer att innehålla ett JSON-svar eller annan data.

## Djupdykning
Grundläggande autentisering är bara en av flera autentiseringsmetoder som kan användas vid HTTP-begäran i Ruby. Andra metoder inkluderar Digest Authentication, OAuth, och SSL-certifikat. Det är viktigt att välja rätt autentiseringsmetod beroende på din användningsfall och säkerhetskrav för din applikation.

## Se även
- [Ruby Net HTTP Dokumentation](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net.html)
- [Grundläggande autentisering på Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Fler autentiseringsmetoder i Ruby](https://blog.engineyard.com/2010/without-rails-http-and-basic-authentication-with-ruby)