---
title:                "Bash: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar med grundläggande autentisering är en vanlig metod för att hantera användarautentisering och säkerhet vid kommunikation mellan en klient och en server. Det är också ett effektivt sätt att skydda känslig data och se till att endast behöriga användare får åtkomst till den.

## Så här

För att skicka en HTTP-förfrågan med grundläggande autentisering i Bash, behöver du använda kommandot ```curl```. Här är ett enkelt exempel på hur man gör det:

```Bash
curl -u användarnamn:lösenord URL
```

I detta kommando anger vi användarnamn och lösenord för autentisering med flaggan ```-u```. Sedan anger vi den URL-adress som vi vill skicka vår förfrågan till. Om autentiseringen lyckas kommer vi att få tillbaka ett svar från servern.

## Djupdykning

Vad händer egentligen bakom kulisserna när vi skickar en HTTP-förfrågan med grundläggande autentisering? Det som händer är att vår klient skickar en förfrågan till servern, men innan den gör det lägger den till ett så kallat “auth header” i vår förfrågan. Denna header innehåller användarnamn och lösenord i en Base64-kodad form. Detta är anledningen till att vi kan skriva dessa uppgifter i klartext i vårt curl-kommando.

En viktig sak att tänka på när man använder grundläggande autentisering är att det inte är den mest säkra metoden, eftersom användarnamn och lösenord skickas i klartext över nätverket. Det finns andra metoder som erbjuder bättre säkerhet, som till exempel OAuth.

## Se även

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
- [Curl man-sida](https://curl.haxx.se/docs/manual.html)
- [Basic Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_access_authentication)