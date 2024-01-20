---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta dess data för lokal användning. Detta görs av programmerare för att analysera, ändra eller lagra information från sidan.

## Hur gör man:
Vi kommer att visa ett enkelt exempel på hur du laddar ner en webbsida med Gleam. Se följande kod:

```Gleam
import gleam/http.{client, Get}
import gleam/string

try response
    = client.get(uri.from_string("https://www.example.com") |> unwrap)
response.status_code
response.body
        |> string.from_utf8
        |> result.unwrap_string
```
I det här exemplet hämtar vi en webbsida (`https://www.example.com`) och skriver ut statuskoden och sidans innehåll som en sträng.

## Djupdykning
Historiskt sett involverade nedladdning av webbsidor ofta att skicka HTTP-begäran och sedan tolka svaret. Med Gleam är processen betydligt enklare, tack vare inbyggda bibliotek som `gleam/http`. 

Bland alternativen finns andra programmeringsspråk och bibliotek. Också asynkrona begäran kan vara en alternativ metod att hämta data från webben.

När det gäller implementation, är grundläggande idén att skicka en GET-begäran till den önskade URL:en, och sedan bearbeta svaret, omvandla bytes till en förståelig sträng med `string.from_utf8`.

## Se även:
- Gleam's officiella webbplats: [här](https://gleam.run/)
- Gleam's http-klient dokumentation: [här](https://hexdocs.pm/gleam_http/gleam/http/0.9.0/) 
- Mer information om HTTP-begäran: [här](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Overview)