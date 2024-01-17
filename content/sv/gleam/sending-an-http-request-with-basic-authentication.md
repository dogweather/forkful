---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Gleam: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering är en metod som används av programmerare för att säkert skicka känslig information över nätverket. Det innebär att begäran måste godkännas med ett användarnamn och lösenord innan den kan utföras.

# Hur man:
Gleam har inbyggda funktioner för att använda grundläggande autentisering i HTTP-begäran. För att skicka en begäran med autentisering, behöver du bara lägga till en autenticeringsheader med användarnamn och lösenord. Se nedan för ett exempel på hur det kan se ut i kod:

```
Gleam.HTTP.post(
    "https://example.com/api/user",
    ~headers=[
        Http.basicAuth("username", "password")
    ]
)
```

Detta kommer att skicka en POST-begäran till URL:en "https://example.com/api/user" med autentiseringsuppgifterna "username" och "password".

# Fördjupning:
Historiskt sett har grundläggande autentisering varit en vanlig metod för att autentisera HTTP-begäran. Men med tiden har flera säkrare alternativ utvecklats, såsom Digest Authentication och OAuth. Det är viktigt att välja rätt autentiseringsmetod baserat på säkerhetsbehovet för din applikation.

När du använder grundläggande autentisering, bör du också vara medveten om att användarnamnet och lösenordet skickas i klartext över nätverket. För att öka säkerheten kan du använda HTTPS för att kryptera begäran och autentiseringsuppgifterna.

# Se även:
För mer information om HTTP-begäran och autentisering, se följande länkar:
- [Gleam Docs: HTTP](https://gleam.run/documentation/#http)
- [MDN Web Docs: Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme) 
- [DigitalOcean: How To Use Basic Authentication With HTTP Requests](https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-with-http-requests)