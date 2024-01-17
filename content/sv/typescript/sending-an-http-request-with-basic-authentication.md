---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "TypeScript: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Vad och Varför?
Att skicka en HTTP-request med grundläggande autentisering innebär att man skickar information till en server för att autentisera sig själv och få åtkomst till skyddade resurser. Programmörer gör detta för att säkerställa att användare som försöker få åtkomst till deras applikation är legitima och har rättigheter att göra det.

Hur görs det?
I TypeScript kan man skicka en HTTP-request med grundläggande autentisering genom att använda en HTTP-klient, som Axios, och ange användarnamn och lösenord i en sökväg eller en header. Det kan se ut så här:
```
TypeScript
let response = await axios.get("/api/resource", { auth: { username: "user", password: "password" } });
console.log(response.data);
```
Detta kommer att skicka ett GET-anrop till "/api/resource" och autentisera det med användarnamn "user" och lösenord "password". Om autentiseringen lyckades kommer svaret från servern att loggas i konsolen.

Djupdykning
Grundläggande autentisering är en inbyggd autentiseringsmetod i HTTP-protokollet och användes ursprungligen för att autentisera HTTP-åtkomst för webbseminarier. Idag används det fortfarande för att säkra tillgång till olika resurser. Det finns dock alternativ, som OAuth, som erbjuder bättre säkerhet och har blivit mer populärt.

När man skickar en HTTP-request med grundläggande autentisering, kodas användarnamn och lösenord i BASE64-format och skickas i en header som heter "Authorization". Detta ger en grundläggande säkerhet, men det är fortfarande inte den mest säkra autentiseringen.

Se även
- [Axios dokumentation om autentisering](https://github.com/axios/axios#authentication)
- [En djupdykning i grundläggande autentisering](https://www.digitalocean.com/community/tutorials/understanding-basic-authentication-in-node-js)