---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:01:53.530665-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att addera användarnamn och lösenord till förfrågan för tillträde till resurser som kräver verifiering. Programmerare gör detta för att säkerställa att endast behöriga användare får tillgång till vissa data eller funktioner.

## Hur man gör:
I Gleam kan HTTP-förfrågningar med grundläggande autentisering skickas genom att använda ett bibliotek som `gleam_http` och lägga till en `Authorization`-header. Här är ett exempel:

```gleam
import gleam/http.{Request, BasicAuth}
import gleam/httpc

fn send_authenticated_request() {
  let auth = BasicAuth(
    username: "användarnamn",
    password: "lösen",
  )
  let request = Request(
    method: Get,
    url: "https://exempel.se/skyddad",
    body: None,
    headers: [auth.header()],
  )
  let response = httpc.send(request)
  response
}
```

Anropet returnerar ett `Response`-objekt som innehåller svaret från servern.

## Djupdykning
Grundläggande autentisering är en enkel autentiseringsmekanism som har använts sedan HTTP-protokollets tidiga dagar. Den är inte den säkraste metoden eftersom användarnamn och lösenord skickas i klartext, kodade med Base64, öppet för potentiell avlyssning. Därför bör HTTPS alltid användas när grundläggande autentisering implementeras.

Alternativt kan programmerare använda modernare metoder som OAuth eller JWT (JSON Web Token) för autentisering. Dessa metoder erbjuder förbättrad säkerhet och flexibilitet.

Implementationen i Gleam kräver att man specificerar autentiseringsuppgifter som headers i HTTP-förfrågan. `BasicAuth` funktionen hjälper till att skapa rätt format av 'Authorization'-headern.

## Se även
- Gleam HTTP documentation: https://hexdocs.pm/gleam_http/
- MDN Web Docs, Grundläggande autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- En introduktion till OAuth 2.0: https://oauth.net/2/
- JWT.io, mer om JSON Web Tokens: https://jwt.io/introduction/
