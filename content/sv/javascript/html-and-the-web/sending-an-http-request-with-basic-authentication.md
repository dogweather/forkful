---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
aliases: - /sv/javascript/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:18.603497-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med basic authentication innebär att man skickar användarnamn och lösenord i klartext, kodat med base64, för att autentisera en användare mot en server. Programmerare gör detta för att begränsa åtkomst till resurser så att endast auktoriserade användare kan nå dem.

## Hur man gör:
För att skicka en HTTP-förfrågan med basic authentication i JavaScript kan du använda 'fetch' funktionen tillsammans med `headers` där du anger 'Authorization'.

```Javascript
// Din användarinfo
const username = 'anvandarnamn';
const password = 'losenord';

// Kodar användarinfo för basic authentication
const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ":" + password));

// Skickar förfrågan
fetch('https://example.com/data', { method: 'GET', headers: headers })
  .then(response => {
    if(response.ok) return response.json();
    throw new Error('Något gick fel vid autentiseringen.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Fel: ', error));
```
Om allt är korrekt ska du se svaret från servern, vanligtvis i JSON-format, i konsolen.

## Fördjupning
HTTP Basic Authentication är en gammal men rak på sak metod för att skicka autentiseringsuppgifter. Det var en del av HTTP/1.0-specifikationen 1996. Även om det är mycket enkelt är det inte det säkraste alternativet eftersom det är lätt att dekryptera base64 och läsa klartextlösenord om inte HTTPS används.

Det finns säkrare alternativ, som OAuth och tokensbaserad autentisering som JWT (JSON Web Token), där användarinformation inte överförs över nätverket utan varje användarsession hanteras med en unik token.

Trots dess säkerhetsbrister används Basic Authentication fortfarande i interna nätverk, scriptade uppgifter, eller enkla API-tester där bekvämlighet tar precedent över säkerhet.

## Se även
- [MDN Web Docs - Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [OWASP - Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)
- [JWT.io - Introduktion till JSON Web Tokens](https://jwt.io/introduction/)
