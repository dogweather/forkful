---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
aliases:
- /no/ruby/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:35.517985-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering betyr å tilføye brukernavn og passord i en forespørsel for å få tilgang til beskyttede ressurser. Programmerere gjør dette for å sikre at kun autoriserte brukere har tilgang.

## Hvordan gjøre det:

```ruby
require 'net/http'
require 'uri'

uri = URI('http://eksempel.no/minhemmeligside')
nett_http = Net::HTTP.new(uri.host, uri.port)

foresporsel = Net::HTTP::Get.new(uri)
foresporsel.basic_auth 'brukernavn', 'passord'

svar = nett_http.request foresporsel

puts svar.body
```

Sample Output:

```
Velkommen til den hemmelige siden!
```

## Dypdykk

Før i tiden var grunnleggende autentisering mye brukt for enkel brukeridentifikasjon. I dag, på grunn av sikkerhetsbegrensninger (det er ikke kryptert), brukes det mindre, ofte i interne systemer eller der dataene ikke er sensitive. 

Alternativer inkluderer OAuth2 og JWT (JSON Web Tokens), begge tilbyr større sikkerhet og er bedre egnet for moderne applikasjoner.

Teknisk sett koder grunnleggende autentisering brukernavn og passord med base64 og legger det til HTTP-forespørselens `Authorization` header. Imidlertid er dette ikke særlig sikkert over HTTP, så det anbefales å bruke HTTPS.

## Se også

- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Bedre sikkerhetspraksis med OAuth: [https://oauth.net/2/](https://oauth.net/2/) 
- JWT for autentisering: [https://jwt.io/](https://jwt.io/)
