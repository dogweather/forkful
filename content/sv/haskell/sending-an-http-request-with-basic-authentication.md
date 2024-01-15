---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Haskell: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering är ett viktigt verktyg för att kommunicera med API:er och andra webbtjänster. Genom att använda grundläggande autentisering kan du säkerställa att bara auktoriserade användare kan få åtkomst till dina resurser och skydda din data från obehörig åtkomst.

## Så här gör du

För att skicka en HTTP-förfrågan med grundläggande autentisering i Haskell, behöver du först importera Network.HTTP.Simple-biblioteket. Sedan kan du använda funktionen `setRequestBasicAuth` för att skapa en anpassad förfrågan med autentiseringsuppgifter.

```Haskell
import Network.HTTP.Simple

request <- setRequestBasicAuth "användarnamn" "lösenord" "http://www.example.com"
```

I detta exempel har vi skapat en förfrågan till URL:en http://www.example.com och använt autentiseringsuppgifterna "användarnamn" och "lösenord". Nu behöver vi bara skicka förfrågan med hjälp av `httpLBS`-funktionen och få ett svar tillbaka.

```Haskell
response <- httpLBS request
print $ getResponseBody response
```

Outputen kommer att ge oss det önskade svar svaret från webbtjänsten. Om autentiseringen misslyckas, kommer vi att få ett felmeddelande i stället.

## Djupdykning

Grundläggande autentisering fungerar genom att inkludera autentiseringsuppgifter i HTTP-förfrågan i form av användarnamn och lösenord. Detta gör att webbtjänsten kan verifiera att användaren har rättigheterna för att få åtkomst till resursen. Detta sker genom att autentiseringsuppgifterna kodas i Base64-format och inkluderas i en HTTP-header.

Det finns flera andra autentiseringsmetoder som kan användas för att skydda HTTP-förfrågningar, men grundläggande autentisering är den enklaste och mest grundläggande metoden. Det är dock viktigt att notera att grundläggande autentisering endast krypterar autentiseringsuppgifterna, inte hela förfrågan, vilket innebär att den fortfarande kan vara sårbar för avlyssning.

## Se även

- [Haskell Network.HTTP.Simple dokumentation](https://hackage.haskell.org/package/http-client-0.5.14/docs/Network-HTTP-Simple.html)
- [HTTP grundläggande autentisering (Wikipedia)](https://sv.wikipedia.org/wiki/HTTP-grundläggande_autentisering)