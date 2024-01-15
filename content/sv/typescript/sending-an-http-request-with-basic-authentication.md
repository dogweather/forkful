---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "TypeScript: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Det finns många tillfällen när det är nödvändigt att skicka en HTTP-begäran med grundläggande autentisering. Det kan vara för att åtkomst till en resurs kräver autentisering eller för att säkra en begäran mot obehörig åtkomst.

## Hur man gör

För att skicka en HTTP-begäran med grundläggande autentisering i TypeScript, används vanligtvis paketet "axios". Detta paket gör det enkelt att göra HTTP-begäranden och kan installeras med kommandot "npm install axios".

Först måste de nödvändiga biblioteken importeras:

```TypeScript
import axios, { AxiosResponse } from "axios";
```

Sedan kan begäran göras med hjälp av följande kod:

```TypeScript
axios.get("https://example.com/api/user", {
  auth: {
    username: "username",
    password: "password"
  }
}).then((response: AxiosResponse) => {
  console.log(response.data);
}).catch((error) => {
  console.log(error);
});
```

I detta exempel görs en GET-begäran till "https://example.com/api/user" med grundläggande autentisering som består av en användarnamn och lösenord. Svaret från begäran loggas sedan till konsolen med hjälp av "console.log()". Om begäran misslyckas loggas istället ett felmeddelande.

## Djupdykning

När en HTTP-begäran görs med grundläggande autentisering, skickas användarnamn och lösenord i klartext över nätverket. Detta kan vara en säkerhetsrisk, särskilt om en osäker anslutning används. För att öka säkerheten kan man istället använda sig av JWT-autentisering (JSON Web Token).

En annan sak att tänka på är att om en begäran ska göras med grundläggande autentisering från en webbläsare, måste den tidigare beskrivna kodändringen utföras på serversidan för att logga in på webbplatsen. Annars kommer det ofta att leda till CORS-fel.

## Se även

- [Axios GitHub-sida](https://github.com/axios/axios)
- [Axios dokumentation](https://github.com/axios/axios#axios) 
- [JWT.io](https://jwt.io/)