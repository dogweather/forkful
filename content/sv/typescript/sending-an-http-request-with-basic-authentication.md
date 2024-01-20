---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Skicka en HTTP-förfrågan med grundläggande autentisering i TypeScript

## Vad & Varför?
Grundläggande autentisering vid HTTP-förfrågningar involverar att sända användarnamn och lösenord i HTTP-huvudet. Den används för att begränsa åtkomsten till vissa resurser på en webbserver till auktoriserade användare.
  
## Hur ska man:
För att skicka en HTTP-förfrågan med grundläggande autentisering kan du använda `axios`, en populär HTTP-klient. Installera `axios` först med npm:

```TypeScript
npm install axios
```

Nu kan du använda `axios` för att skicka en HTTP-förfrågan med grundläggande autentisering:

```TypeScript
import axios from 'axios';

const config = {
  auth: {
    username: 'username',
    password: 'password'
  }
};

axios.get('https://mywebsite.com', config)
    .then((response) => {
        console.log(response.data);
    })
    .catch((error) => {
        console.error(error);
    });
```

## Djup dykning
Grundläggande autentisering i HTTP introducerades i HTTP/1.0-specifikationen på 90-talet. Trots att den är grundläggande och enkel att implementera, saknar den många säkerhetsfunktioner. För att hantera dessa brister, kan du överväga alternativ som OAuth eller JWT för mer robust säkerhet.

Vid användning av grundläggande autentisering skickas dina autentiseringsuppgifter i klartext som en del av varje förfrågan. Det innebär att om någon lyckas avlyssna din kommunikation kan de lätt se ditt lösenord. Använd alltid HTTPS när du använder grundläggande autentisering för att kryptera dina autentiseringsuppgifter.

## Se även
För mer information om HTTP autentiferingsmekanismer, kolla in dessa länkar:

- MDN Web Docs: [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- GitHub: [Axios](https://github.com/axios/axios)
- OAuth: [OAuth 2.0](https://oauth.net/2/)
- JWT: [JSON Web Tokens](https://jwt.io/)