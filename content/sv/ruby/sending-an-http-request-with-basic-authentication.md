---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Ruby: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTP-förfrågan med grundläggande autentisering är vanligt när man vill säkra en webbplats eller API med ett enkelt användarnamn och lösenord.

## Hur man gör det
Först behöver vi installera den nödvändiga Ruby-modulen Net::HTTP som ger oss möjlighet att skicka HTTP-förfrågningar. Vi användar sedan följande kod för att skapa en autentiseringssträng med användarnamn och lösenord:
```ruby
require 'net/http'
require 'uri'
uri = URI.parse("https://example.com")
auth = Net::HTTP::BasicAuth.new("username", "password")
```
Vi kan sedan skapa och skicka en GET-förfrågan med hjälp av vår autentiseringssträng:
```ruby
Net::HTTP.start(uri.host, uri.port, use_ssl: true) do |http|
  request = Net::HTTP::Get.new(uri)
  request.basic_auth(uri.user, uri.password)
  response = http.request(request)
  puts response.body
end
```
Detta kommer att ge oss en utmatning med innehållet på den autentiserade webbplatsen eller API:en.

## Djupdykning
När vi skickar en HTTP-förfrågan med grundläggande autentisering skickas användarnamn och lösenord i klartext, vilket innebär att det inte är den säkraste autentiseringsmetoden. Det rekommenderas att använda HTTPS istället för HTTP för att kryptera förfrågan och göra den svårare att avlyssna. Det är också viktigt att använda starka lösenord och inte dela dem med andra för att säkerställa att vår autentisering är effektiv.

## Se även
- [Ruby Net::HTTP dokumentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Net::HTTP::BasicAuth dokumentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/BasicAuth.html)
- [HTTP-kryptering för webbutvecklare](https://developer.mozilla.org/sv/docs/Web/Security/Sensitive_information_in_URLs#http_f%C3%B6r_idag_och_igen)