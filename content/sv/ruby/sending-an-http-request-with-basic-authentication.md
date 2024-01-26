---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:02:39.753224-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering betyder att du förmedlar användarnamn och lösenord säkert för att få tillgång till en resurs på webben. Programmerare gör detta för att interagera med webbtjänster som kräver inloggning.

## Hur man gör:
```ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/secured/resource')
username = 'anvandare'
password = 'lösenord'

# Skapar ett HTTP-objekt och begär med grundläggande autentisering
Net::HTTP.start(uri.host, uri.port) do |http|
  request = Net::HTTP::Get.new(uri)
  request.basic_auth(username, password)

  response = http.request(request)
  puts response.body
end
```
Om svarskoden är `200`, blev förfrågan framgångsrik: 
```
Välkommen, du är nu inloggad.
```
Om `401`, misslyckades autentisering:
```
Ogiltig autentisering.
```

## Fördjupning:
Skicka HTTP-förfrågan med grundläggande autentisering har sitt ursprung i HTTP/1.0 och har följt med till HTTP/1.1. Det är standard men anses inte super-säkert, eftersom användarnamn och lösenord kodas med base64, vilket är lätt att dekoda. I modern tid används ofta robustare autentiseringssystem som OAuth. För grundläggande autentisering i Ruby, Net::HTTP-biblioteket kan hantera förfrågningar men för mer avancerade saker, överväg att använda bibliotek som RestClient eller HTTParty som smidigarbetet.

## Se Även:
- Ruby's Net::HTTP dokumentation: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty GitHub-resurs: https://github.com/jnunemaker/httparty
- RestClient dokumentation: https://github.com/rest-client/rest-client
