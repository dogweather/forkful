---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka HTML innebär att omvandla HTML-strängar till något mer användbart för ditt program, till exempel Gleams trästruktur. Programmers gör detta för att plocka ut och interagera med data i HTML-dokument.

## Hur till:

Här är ett exempel på hur du kan tolka HTML med Gleam:

```Gleam
import gleam/http.{HttpClient}
import gleam/string_builder.{Builder}

fn html_tolk() {
  let http = HttpClient.To.apply()
  let url = "http://example.com"
  
  let respons = HttpClient.get(http, url)
  let html_sträng = Builder.to_string(respons.body)

  // Din HTML-tolkning logik här
  // ...
}
```

Output:

```Gleam
"<!doctype html>..."
```

## Fördjupning 

Historiskt sett, HTML-tolkning har använts länge för webbskrapning och att få data från webbsidor. Det finns alternativ till HTML-tolkning, som inkluderar API-anrop och JSON-analys.

Men, HTML-tolkning tillåter interaktion med webbsidor på ett mycket mer detaljerat sätt. I Gleam, HTML-tolkning drivs av Gleams fantastiska sträckor, vilket gör att HTML-dokument kan representeras som inbäddade listor och lattelefoner.

## Se även 

- [W3Schools Tutorial på HTML DOM Traversal & Manipulation](https://www.w3schools.com/js/js_htmldom_navigation.asp)
- [Mozilla dokumentation om HTML tolkning](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [Gleam dokumentation](https://gleam.run/docs/introduction/)