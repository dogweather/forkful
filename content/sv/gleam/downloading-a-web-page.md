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

Varför: Varför Skulle Du Vilja Ladda Ner En Webbplats?

Ibland kan det vara användbart att kunna ladda ner en webbplats för offline användning eller för att analysera dess innehåll. Det kan också vara ett sätt att spara favoritwebbplatser som du vill besöka igen senare, även om de skulle tas bort från internet.

## Så här gör du

För att ladda ner en webbplats med Gleam, används funktionen `httpc.get` tillsammans med funktionen `Gleam.IO.File.write` för att spara den nedladdade sidan som en fil. Här är ett enkelt exempel som laddar ner Glesams hemsida och sparar den som en fil med namnet "gleam.html":

```Gleam
let request = httpc.get("https://gleam.io")
request
  .then(Response.body)
  .and_then { body -> File.write("gleam.html", body) }
  .panic
```

För att öppna den nedladdade filen i din webbläsare kan du använda funktionen `os.open_browser("gleam.html")`.

## En djupdykning

Med hjälp av Gleams funktioner kan du anpassa hur du vill ladda ner en webbplats. Till exempel kan du ange specifika headern för din webbläsare genom att använda `httpc.set_header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0 Safari/605.1.15")` innan du utför din förfrågan.

Du kan också använda `httpc.post` istället för `httpc.get` för att ladda ner sidor som kräver inloggning eller annan form av interaktion.

## Se också

- Mer information om Gleams HTTP-funktionalitet: https://gleam.run/docs/http
- Provrördet för `httpc.get`: https://gleam.run/provroersel/http/httpc.get
- Provrördet för `Gleam.IO.File.write`: https://gleam.run/provroersel/file/file.write