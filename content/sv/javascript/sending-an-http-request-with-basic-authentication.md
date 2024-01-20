---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Skicka en HTTP-begäran med grundläggande autentisering i JavaScript
## Vad & Varför?
Skicka en HTTP-begäran med grundläggande autentisering handlar om att skicka data till en server med en autentiseringsheader. Programmerare gör det för att säkerställa att endast de behöriga får åtkomst till dessa data.

## Så här gör du:
För att Skicka en HTTP-begäran med grundläggande autentisering kan du använda Node.js biblioteket Axios.

```Javascript
const axios = require('axios').default;
const username = 'username';
const password = 'password';

axios({
  method: 'get',
  url: 'https://example.com/data',
  auth: {
    username: username,
    password: password
  }
}).then(response => {
  console.log(response.data);
}).catch(error => {
  console.error(error);
});
```

Programmet skickar en begäran till `https://example.com/data` med användarnamn och lösenord. Svaret från servern (datan) kommer att skrivas ut i konsolen.

## Djupdykning
Grundläggande autentisering har varit en del av HTTP/1.0 sedan 1996. Dess popularitet beror på dess enkelhet, men det har begränsningar, som att det är osäkert över osäkrade nätverk.

Alternativ till grundläggande autentisering inkluderar OAuth och JWT, som ger utökad säkerhet och funktionalitet.

Vid implementering, använd alltid HTTPS för att säkerställa att dina autentiseringsuppgifter är säkra. Observera att lösenordet för grundläggande autentisering skickas som en base64-kodad sträng, vilket inte är särskilt säkert.

## Se också
- [Axios Dokumentation](https://axios-http.com/docs/intro)
- [Bearer Authentication](http://www.ietf.org/rfc/rfc6750.txt)
- [JWT Authenticaction](https://jwt.io/introduction/)