---
title:                "Sända en http-begäran"
html_title:           "Gleam: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP förfrågan är när en utvecklare ber sin dator att kontakta en annan dator och begära information från den. Det är ett viktigt verktyg för utvecklare eftersom det låter dem integrera och hämta data från externa källor i sina applikationer.

## Hur man gör:
```Gleam
// Exempel på en HTTP förfrågan med Gleam
let resultat = Http.request(
  url: "https://example.com/api/posts",
  method: Http.post,
  headers: [( "Content-Type", "application/json")],
  body: "{\"title\":\"Hello\",\"content\":\"My first post\"}"
)
// Output: En bekräftelse att förfrågan har skickats och eventuell data som svar.
```

## Djupdykning:
Att skicka en HTTP förfrågan har funnits sedan starten av webben. Det finns andra alternativ för att integrera med externa källor, som till exempel att använda WebSocket för kontinuerligt utbyte av data. I Gleam finns det en inbyggd HTTP modul som bygger på Erlangs standardbibliotek för att skicka förfrågningar. 

## Se också:
- [Gleams HTTP dokumentation](https://gleam.run/posts/http/)
- [Erlangs HTTP modul](https://erlang.org/doc/man/http.html)