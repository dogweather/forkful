---
title:                "Skicka en http-förfrågan"
html_title:           "Gleam: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför?

Att skicka HTTP-förfrågningar är en viktig del av många webbapplikationer och servrar. Genom att kunna skicka och ta emot HTTP-förfrågningar kan din applikation kommunicera med andra system och ge användarna en naturlig och smidig upplevelse. 

## Hur man gör det

För att skicka en HTTP-förfrågan i Gleam använder man inbyggda moduler som {httpc} och {request}. Här är ett exempel på hur man kan skicka en GET-förfrågan med hjälp av {httpc} modulen och få tillbaka ett svar:

```Gleam
import httpc.{Request, get}
import httpc.Response

let url = "https://example.com"

let request = Request(
  method: get, 
  url: url
)

let response =
  case httpc.send(request) {
    Ok(resp) -> resp
    Error(e) -> error("HTTP request failed: {e}")
  }

HttpResponse.print(response)
```

Output:
```
{"status": "200 OK", "response_headers": {"content_type": "text/html", "content_length": "606", "server": "Apache/2.4.48"}, "body": "<html>...</html>"} 
```

## Djupdykning

HTTP-förfrågningar består av olika delar, som request-line, request headers och request body. I Gleam kan man konfigurera dessa delar genom att sätta olika värden i request-objektet. Man kan också använda sig av {request} modulen för att skicka mer avancerade förfrågningar som kräver autentisering eller som skickar data i ett specifikt format som JSON. Det är viktigt att förstå dessa koncept för att kunna skicka korrekta och effektiva HTTP-förfrågningar. 

## Se också

- [Gleam HTTPc modul](https://gleam.run/modules/httpc/)
- [Gleam request modul](https://gleam.run/modules/request/)
- [En guide för att arbeta med HTTP-förfrågningar i Gleam](https://gleam.run/articles/http-requests/)