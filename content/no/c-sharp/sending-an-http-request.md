---
title:                "Å sende en http-forespørsel"
html_title:           "C#: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En HTTP-request er en måte å be om informasjon fra en nettside eller web-tjeneste. Det er en fundamental del av oppbygningen av internettet, da det lar programmerere sende og motta data mellom enheter. Vanligvis bruker man HTTP-protokollen for å sende disse forespørslene og motta svar.

## Slik gjør du det:
I C# kan man sende en HTTP-request ved å bruke klassen HttpWebRequest i System.Net namespace. Her er et eksempel på hvordan det kan gjøres:
```C#
// Oppretter en request til en URL
HttpWebRequest request = (HttpWebRequest)WebRequest.Create("https://www.example.com");
// Sender requesten og får en response tilbake
HttpWebResponse response = (HttpWebResponse)request.GetResponse();
// Leser data fra response stream
string data = new StreamReader(response.GetResponseStream()).ReadToEnd();
// Skriver ut data til konsoll
Console.WriteLine(data);
```
Åpne denne koden i et tekstredigeringsprogram og bytt ut URL-en med en faktisk nettside for å få en output til konsollen med data fra den nettsiden.

## Dykk dypere:
HTTP-protokollen ble utviklet på 90-tallet og har blitt en standard for å kommunisere mellom klienter og servere på nettet. Det finnes også andre måter å sende og motta data, som for eksempel FTP, SMTP og WebSocket.

Det finnes også ulike biblioteker og rammeverk som kan brukes til å sende og håndtere HTTP-requests, som for eksempel .NET HttpClient og RestSharp.

Når man sender en HTTP-request, blir den delt inn i flere deler som request line, header og content. Disse delene beskriver hva slags informasjon som blir sendt og hvordan den skal behandles. Det er viktig å være kjent med disse delene for å kunne lage en korrekt HTTP-request.

## Se også:
Les mer om HTTP på [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP).

Utforsk ulike biblioteker for å håndtere HTTP-requests, som [RestSharp](https://restsharp.dev/) og [Flurl](https://flurl.dev/).

Lær hvordan du kan bruke HTTP-requests i praksis ved å lage et API-kall i C# ved hjelp av [Postman](https://www.postman.com/).