---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att vi ber om resurser från en webbserver med användarnamn och lösenord. Programmerare gör detta för att åtkomst till vissa resurser endast borde ges till auktoriserade individer.

## Hur man gör:

Här visar vi hur man skickar en HTTP-begäran med grundläggande autentisering i Ruby med hjälp av `net/http`-biblioteket:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'pass'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}
puts res.body
```

I koden ovan skapar vi först en `URI`-instans. Sedan bygger vi en HTTP GET-förfrågan med grundläggande autentisering, sätter kontoinformationen till 'user' och 'pass'. Till sist skickar vi begäran och skriver ut responskroppen.

## Djupdykning

Grundläggande autentisering är en äldsta metod som används för att skydda webbresurser. Men det ger inte stark säkerhet och bör endast användas med HTTPS för att förhindra att autentiseringsuppgifter fångas upp.

Som alternativ till `net / http`, finns det andra bibliotek såsom Faraday och HTTParty, som erbjuder mer funktionella gränssnitt för att skicka HTTP-förfrågningar. För komplicerade användsfall kan du också överväga att använda OAuth.

När du skickar en förfrågan med grundläggande autentisering, läggs en `Authorization`-rubrik till HTTP-förfrågan med 'Basic' följt av bas64-kodade kontoinformationen.

## Se också

- [Ruby Basic Authentication](https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html#class-Net::HTTP-label-Basic+Authentication)
- [Faraday](https://lostisland.github.io/faraday)
- [HTTParty](https://github.com/jnunemaker/httparty)
- [OAuth](https://oauth.net/)
- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication) på Wikipedia.