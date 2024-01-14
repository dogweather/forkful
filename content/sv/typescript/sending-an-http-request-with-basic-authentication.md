---
title:                "TypeScript: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTP-förfrågan med grundläggande autentisering är en vanlig uppgift inom webbutveckling. Det kan till exempel användas för att autentisera användare vid inloggning eller för att hämta data från en API-tjänst. I denna artikel kommer vi att titta på hur vi kan implementera detta i TypeScript.

## Så här gör du
För att skicka en HTTP-förfrågan med grundläggande autentisering i TypeScript behöver vi först importera nödvändiga moduler. Vi kan använda ```axios```-modulen för att göra förfrågningar och ```Buffer```-modulen för att koda lösenordet i Base64-format. Vi kan sedan använda följande kod för att göra en GET-förfrågan med autentisering:

```TypeScript
import axios from 'axios';
import { Buffer } from 'buffer';

const username = 'användarnamn';
const password = 'lösenord';

const auth = 'Basic ' + Buffer.from(username + ':' + password).toString('base64');

axios.get('https://www.example.com/api', { headers: { Authorization: auth } })
  .then(response => {
    console.log(response.data);
  })
  .catch(err => {
    console.log(err);
  });
```

I detta exempel använder vi ```Buffer``` för att koda användarnamn och lösenord i Base64-format och lägger sedan till det i en ```Authorization```-header i vår förfrågan. Beroende på vilken HTTP-metod vi använder och vilken typ av förfrågan vi gör, kan vi behöva ändra koden enligt våra behov. Det är också viktigt att notera att vi bör undvika att hårdkoda lösenordet och istället använda en säkrare metod för att hantera autentiseringsuppgifter.

## Djupare dykning
Vid grundläggande autentisering skickar klienten användarnamn och lösenord i varje förfrågan. Detta gör det enklare att autentisera, men medför också en säkerhetsrisk eftersom autentiseringsuppgifterna kan avlyssnas om förfrågan inte är krypterad. Detta är anledningen till att grundläggande autentisering ofta används tillsammans med HTTPS-protokollet, som krypterar förfrågningar och svar mellan klienten och servern.

## Se även
- [Axios dokumentation](https://github.com/axios/axios)
- [Buffer dokumentation](https://nodejs.org/api/buffer.html) 
- [HTTP-autenticering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)