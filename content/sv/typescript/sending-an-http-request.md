---
title:                "TypeScript: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar är en viktig del av webbutveckling. Det är ett sätt att kommunicera med servrar och få tillgång till data och resurser. Utan HTTP-förfrågningar skulle det vara nästintill omöjligt att skapa dynamiska och interaktiva webbsidor.

## Hur man gör det

För att skicka en HTTP-förfrågan i TypeScript behöver vi använda oss av ett inbyggt bibliotek som heter "http". Låt oss titta på hur vi kan göra en GET-förfrågan till en webbadress och logga svaret i konsolen:

```
import * as http from 'http';

http.get('www.example.com', (response) => {
  console.log(`Statuskod: ${response.statusCode}`); // Detta kommer att logga statuskoden, t.ex. "200" om förfrågan lyckades.
  response.on('data', (chunk) => {
    console.log(`Data mottagen: ${chunk}`); // Detta kommer att logga den data som returnerades från webbadressen.
  });
}).on('error', (error) => {
  console.error(`Får en error: ${error.message}`); // Detta kommer att logga eventuella fel som kan uppstå.
});
```

Output:

```
Statuskod: 200
Data mottagen: <h1>Hej!</h1>
```

Som vi kan se måste vi först importera http-biblioteket och sedan använda dess `get()`-funktion för att göra en GET-förfrågan till den önskade webbadressen. Vi behöver också ange en callback-funktion som kommer att hantera svaret från servern. I vårt exempel använder vi `console.log()` för att logga både statuskoden och den returnerade datan.

I vårt `get()`-anrop kan vi också specificera eventuella parameterar, t.ex. query strings eller headerns. Vi kan även använda liknande funktioner för andra typer av HTTP-förfrågningar som POST och PUT.

## Djupdykning

Det finns många olika anledningar till varför vi kan behöva skicka HTTP-förfrågningar i TypeScript. Det kan vara för att hämta data från en API, skicka och ta emot webbformulär eller kommunicera med en server i realtid med hjälp av websockets. Oavsett vad det är så är HTTP-förfrågningar ett oumbärligt verktyg inom webbutveckling.

När vi skickar en HTTP-förfrågan skickar vi också med en header som innehåller relevant information som t.ex. vilken typ av data vi förväntar oss tillbaka och vilken metod vi använder. Detta är viktigt för att servern ska kunna behandla förfrågan på rätt sätt.

Det finns också flera olika statuskoder som kan returneras från servern efter en förfrågan. Dessa koder ger oss information om hur förfrågan har hanterats, t.ex. om den lyckades eller om det har uppstått ett fel.

## Se även

- [HTTP-standarden](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
- [Axios](https://github.com/axios/axios)
- [Superagent](https://github.com/visionmedia/superagent)