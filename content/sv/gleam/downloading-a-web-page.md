---
title:                "Ladda ner en webbsida"
html_title:           "Gleam: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta en webbsida är en process där man laddar ner en kopia av en webbsida från internet till en dator eller annan enhet. Detta görs vanligtvis av programmerare för att kunna bearbeta och använda data från webbsidor i sina program.

## Så här gör du:
För att hämta en webbsida i Gleam kan man använda funktionen `httpc.get(url)` från biblioteket `gleam/httpc`. Detta kommer att returnera en `Result` med antingen en `Ok` med en `HttpResponse` som innehåller den hämtade sidan, eller ett `Error` om något gick fel. Exempel:

```Gleam
import gleam/httpc

let result = httpc.get("https://www.example.com")

case result {
  Ok(response) ->
    case response.body {
      Ok(body) ->
        // Gör något med body

      Err(error) ->
        // Hantera fel
    }

  Err(error) ->
    // Hantera fel
}
```

## Djupdykning:
Att hämta webbsidor är en viktig del av många programmerares arbete, särskilt för webbutveckling och dataanalys. Det finns alternativa lösningar för att hämta webbsidor, såsom att använda verktyg som cURL eller bibliotek från andra programmeringsspråk. Implementeringen av `httpc.get` använder sig av ett bibliotek som heter Mochi, som hanterar HTTP-anrop och returnerar resultatet som en `Result`.

## Se även:
Läs mer om Gleam på deras officiella webbplats: https://gleam.run/