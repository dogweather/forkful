---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Ruby: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att man skickar en begäran till en webbserver och tillhandahåller autentiseringsuppgifter för att verifiera sin identitet. Programmers gör detta för att kunna få åtkomst till skyddade resurser på en server.

## Hur man gör:
Här är ett exempel på hur man skickar en HTTP-begäran med grundläggande autentisering i Ruby:

```ruby
require 'uri'
require 'net/http'

# Skapa en URI för begäran
url = URI('https://example.com/api')

# Skapa en Net::HTTP-anslutning till URL:en
http = Net::HTTP.new(url.host, url.port)

# Skapa en begäran med önskad metod och tillagd autentisering
request = Net::HTTP::Get.new(url)
request.basic_auth('användarnamn', 'lösenord')

# Skicka begäran och spara svaret
response = http.request(request)

# Skriv ut statuskoden och svaret
puts "Statuskod: #{response.code}"
puts "Svar: #{response.body}"
```

Detta kommer att skicka en GET-begäran till `https://example.com/api` med autentiseringsuppgifterna "användarnamn" och "lösenord". Svaret, inklusive statuskoden, kommer att skrivas ut i terminalen.

## Djupdykning:
Att skicka en HTTP-begäran med grundläggande autentisering är en mycket vanlig metod för att skydda resurser på en server. Den finns också andra autentiseringsmetoder som OAuth och tokenbaserad autentisering, men grundläggande autentisering är enkelt att implementera och fungerar bra för många scenarion.

En grundläggande autentiseringsbegäran består av en användarnamn och ett lösenord, som kodas i Base64-format och bifogas begäran som en HTTP-header. Detta ger en grundläggande nivå av skydd, men det är fortfarande viktigt att använda HTTPS för att kryptera all kommunikation mellan klienten och servern.

## Se även:
Här är några bra källor för att lära dig mer om att skicka HTTP-begäran med grundläggande autentisering:

- [Officiell dokumentation för Net::HTTP i Ruby] (https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [En guide för att lägga till grundläggande autentisering i en Ruby-applikation] (https://www.twilio.com/blog/2017/03/http-basic-authentication-in-ruby.html)
- [En djupare förklaring av grundläggande autentisering] (https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)