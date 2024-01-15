---
title:                "Sända en http-begäran"
html_title:           "TypeScript: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

För att skicka en förfrågan över HTTP använder vi vanligtvis ett programmeringsspråk som heter TypeScript. Genom att skicka en HTTP-förfrågan kan vi få information från en server, till exempel om en webbsida eller ett API.

## Hur man gör det

För att skicka en HTTP-förfrågan i TypeScript behöver vi först importera biblioteket "http" som är en del av Node.js. Sedan använder vi funktionen "http.request()" för att skicka vår förfrågan. Nedan är ett exempel som skickar en GET-förfrågan till en API-adress och loggar svaret till konsolen:

```TypeScript
import http from 'http'

const options = {
  hostname: 'api.example.com',
  path: '/users',
  method: 'GET'
}

const request = http.request(options, (response) => {
  let data = ''

  response.on('data', (chunk) => {
    data += chunk
  })

  response.on('end', () => {
    console.log(data)
  })
})

request.end()
```

Detta kodblock skapar en förfrågan med metoden GET till adressen "api.example.com/users" och lyssnar sedan på svaret. När svaret är klart skriver vi ut det med hjälp av console.log(). På detta sätt kan vi enkelt få tillgång till data från en annan server.

## Djupdykning

När vi skickar en HTTP-förfrågan är det viktigt att vi förstår vad det är vi skickar. En HTTP-förfrågan består vanligtvis av en metod (som GET eller POST), en väg (som "/users"), samt eventuella parametrar och en headers. Det är också viktigt att vi får rätt typ av svar tillbaka, till exempel i JSON-format om vi förväntar oss data.

För att skicka förfrågningar till servern behöver vi också lära oss mer om asynkron programmering, vilket innebär att vi skickar flera förfrågningar samtidigt och tar emot svaren när de är klara. Det kan också vara användbart att använda sig av ett paket som heter "axios" som underlättar hanteringen av HTTP-förfrågningar och ger oss enklare kod.

## Se även

- [Axios](https://github.com/axios/axios)
- [Understanding Asynchronous Programming in TypeScript](https://www.digitalocean.com/community/tutorials/understanding-asynchronous-programming-in-typescript-sv)
- [Node.js - HTTP modulen](https://nodejs.org/api/http.html#http_http_request_options_callback)