---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att skicka en HTTP-begäran med grundläggande autentisering är att göra en förfrågan till en webbserver från din kod, samtidigt som du förser den med autentiseringsuppgifter. Programmerare gör detta för att få åtkomst till skyddade resurser på servern.

## Hur Man Gör: 
Här är ett litet exempel på hur du kan skicka en HTTP-begäran med grundläggande autentisering i Gleam:

```Gleam
import gleam/http.{get, header, request}
import gleam/codecs.base64.{encode}

let credentials = "username:password"
let encoded = tuple(Ok: encode(credentials))

let authentication_header = case encoded {
  Ok(encoded) -> header.custom("Authorization", "Basic " ++ string.from_result(encoded))
  Error(_) -> … // hantera fel här
}

let request = request.new("http://www.example.com")
  |> request.prepend_header(authentication_header)

http.send(request)
```

Märk: Testkör din kod och ersätt "username:password" med dina riktiga autentiseringsuppgifter.

## Fördjupning
Historiskt sett, HTTP Basic Authentication skapades som en del av HTTP/1.0 specifikationen. Även om det är snabbt och enkelt att använda, så är det inte det mest säkra sättet att autentisera begäranden eftersom lösenord skickas som klartext. 

För säkrare alternativ, överväg att använda OAuth, tokenbaserad autentisering eller JWT. Dock, HTTP Basic Authentication kan fortfarande vara tillräckligt för interna applikationer eller för testning och utveckling.

I din Gleam-kod hanteras autentiseringen genom att skapa ett "Authorization"-huvud med base64-kodade autentiseringsuppgifter och lägga till det i HTTP-begäran.

## Se Även 
För mer information, kolla in följande resurser:
- [Gleam HTTP Documentation](https://gleam.run/book/tour/http.html)
- [MDN: HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [RFC 7617: 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)