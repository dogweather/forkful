---
title:                "Gleam: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

#Varför
 Grundläggande autentisering är en vanlig metod för att skydda webbutvecklingsprojekt från obehörig åtkomst. Genom att lära dig hur man skickar HTTP-förfrågningar med grundläggande autentisering kan du säkert kommunicera med externa API:er och tjänster, vilket är avgörande för många webbapplikationer.

#Så här gör du
 För att skicka en HTTP-förfrågan med grundläggande autentisering i Gleam, behöver du först importera biblioteket för HTTP-anrop:
 
```Gleam
import http
```
 
Nästa steg är att skapa ett anrop-objekt med hjälp av funktionen "http.request":
 
```Gleam
let req = http.request(
    method=Get,
    url="http://api.exempel.com/",
    headers=[("Authorization", "Basic QWxhZGRmqndMNtJZkSnd=")]
)
```

I exemplet ovan har vi skapat ett GET-anrop till en API-adress och lagt till en grundläggande autentiseringsrubrik med användarnamn och lösenord i Base64-kodning.

Nästa steg är att faktiskt skicka förfrågan och hantera svaret:
 
```Gleam
let res = http.send(req)

match res {
    Ok(_) -> "Förfrågan skickad."
    Error(_) -> "Något gick fel."
}

match res {
    Ok(resp) ->
        let body = http.read_body(resp)
        "Svar från API: " ++ body
    Error(_) -> "Något gick fel."
}
```

I exemplet ovan använder vi "http.send" för att skicka förfrågan och sedan matchar vi svaret för att få reda på om förfrågan lyckades eller inte. Om den lyckas, kan vi använda "http.read_body" för att läsa svarets innehåll och arbeta med det på lämpligt sätt.

#Djupdykning
Det finns en mängd olika bibliotek och metoder för att hantera HTTP-förfrågningar med grundläggande autentisering i Gleam. Du kan till exempel använda biblioteket "openssl" för att hantera SSL-certifikat vid autentisering, eller använda "base64" för att enkelt koda och avkoda användarnamn och lösenord i Base64.

#Se även
- Gleam-dokumentation för HTTP-anrop: https://gleam.run/libraries/http/
- OpenSSL-biblioteket för Gleam: https://github.com/lpil/gleam-openssl
- Base64-biblioteket för Gleam: https://github.com/stenington/base64