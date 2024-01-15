---
title:                "Skicka en http-begäran med grundläggande autentisering."
html_title:           "Gleam: Skicka en http-begäran med grundläggande autentisering."
simple_title:         "Skicka en http-begäran med grundläggande autentisering."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Det är vanligt att behöva skicka HTTP-förfrågningar med grundläggande autentisering för att kommunicera med webbtjänster och API:er. Detta kan krävas när du behöver säkra och skyddade förbindelser mellan din applikation och en extern server. Grundläggande autentisering är en enkel och utbredd autentiseringsmetod som använder en användarnamn-lösenord-kombination för att verifera identiteten hos den som skickar förfrågan.

## Så här gör du

För att skicka en HTTP-förfrågan med grundläggande autentisering i Gleam, behöver du först importera det inbyggda biblioteket för HTTP-förfrågningar. Sedan anropar du funktionen `request` med rätt parametrar för metod, URL och eventuellt payload-data. Du kan sedan inkludera din autentiseringsinformation i förfrågningsheadern genom att lägga till en `Authorization`-header med en bas64-kodad sträng som innehåller användarnamn och lösenord.

```Gleam
import gleam/http

pub fn send_request() {
    let response = http.request(
        method: "GET",
        url: "https://example.com/api",
        headers: [
            ("Authorization", "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
        ]
    )
}
```

När du får tillbaka ett svar från servern kan du hantera det som en vanlig Gleam-uppdelning för att få tillgång till svarskoden, rubrikerna och eventuell innehållsdata. Genom att använda returtypen `http.response` kan du också få tillgång till en mer detaljerad representation av hela svarspaketet.

```Gleam
import gleam/http

pub fn handle_response(response) {
    let { status, headers, body } = response
    // Gör något med svarskoden, rubrikerna och innehållet
}

pub fn handle_full_response(response) {
    let { status, headers, body } = response
    let http_response = http.response(status, headers, body)
    // Gör något med den mer detaljerade representationen
}
```

## Djupdykning

När du skickar en förfrågan med grundläggande autentisering, är det viktigt att komma ihåg att lösenordet skickas som en bas64-kodad sträng, vilket betyder att det inte är säkert. Därför bör du undvika att använda grundläggande autentisering vid kritiska förfrågningar och istället överväga en starkare autentiseringsmetod som till exempel OAuth.

En annan sak att vara medveten om är att det är möjligt att använda HTTPS tillsammans med grundläggande autentisering för en extra säkerhetsnivå. När du använder HTTPS krypteras både förfrågan och autentiseringsinformationen som skickas över nätverket, vilket gör det svårare för någon att få tag på dina autentiseringsuppgifter.

## Se även

- [HTTP Clients in Gleam](https://gleam.run/articles/http-clients/)
- [Authenticate HTTP requests using Basic auth in Gleam](https://gist.github.com/wintondeshong/0329ab0d7aea9a146bdeec3ad38cbc96)