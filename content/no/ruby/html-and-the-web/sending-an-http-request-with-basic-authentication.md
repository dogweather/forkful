---
date: 2024-01-20 18:02:35.517985-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ \xE5 tilf\xF8ye brukernavn og passord i en foresp\xF8rsel for \xE5 f\xE5 tilgang\
  \ til beskyttede\u2026"
lastmod: '2024-03-13T22:44:41.327670-06:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ \xE5 tilf\xF8ye brukernavn og passord i en foresp\xF8rsel for \xE5 f\xE5 tilgang\
  \ til beskyttede ressurser."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

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
