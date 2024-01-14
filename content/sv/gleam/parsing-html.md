---
title:                "Gleam: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

HTML-analys (även känd som HTML-parsning) är en viktig uppgift för utvecklare inom webbutveckling och automatisering. Det innebär att extrahera information från HTML-kod för att manipulera eller använda det på ett något annat sätt. Det kan användas för att skapa webbskrapor, datautvinning, automatiserad testning och mycket mer. Det är ett viktigt verktyg att ha i din utvecklarverktygslåda.

## Så här

Att analysera HTML med Gleam är förvånansvärt enkelt. Först och främst måste vi importera nödvändiga moduler, som `html`, `http.client` och `gleam/json`:

```
import html
import http.client
import gleam/json
```

Sedan behöver vi en HTML-sträng att analysera. I det här fallet använder vi vår egen hemsida:

```
let html_string = http.client.get("https://www.example.com")
|> html.stringify
```

Nu kan vi använda `html.parse`-funktionen för att omvandla vår HTML-sträng till en JSON-representasjon:

```
let parsed = html.parse(html_string)
|> gleam/json.encode_pretty(2)
```

I exemplet ovan använder vi också `gleam/json.encode_pretty`-funktionen för att förbättra läsbarheten av vår JSON-utdata genom att formatera den med ett indenteringssteg på 2.

Efter att ha kört detta får vi följande utdata:

```
{
  "name": "html",
  "attributes": {},
  "children": [
    {
      "name": "head",
      "attributes": {},
      "children": [
        { "name": "title", "attributes": {}, "children": ["Example Domain"] }
      ]
    },
    {
      "name": "body",
      "attributes": {},
      "children": [
        { "name": "h1", "attributes": {}, "children": ["Example Domain"] },
        {
          "name": "p",
          "attributes": {},
          "children": ["This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission."]
        }
      ]
    }
  ]
}
```

Som du kan se har vår HTML-sträng nu blivit strukturerad som en JSON-fil, vilket gör det lättare att arbeta med den och extrahera information från den.

## Djupdykning

Att lära sig om HTML-analys är inte bara användbart för webbutveckling och automatisering, det är också en viktig kunskap för säkerhet. Genom att förstå hur HTML-kod fungerar kan vi identifiera potentiella säkerhetsrisker i våra egna webbsidor och skydda oss mot skadlig kod.

Vid djupdykningen i HTML-analys kan vi också utforska mer avancerade funktioner, som att använda CSS-väljare för att välja specifika element i en HTML-sida eller extrahera attributvärden från HTML-taggar.

## Se också

- [Gleam dokumentation för HTML-modulen](https://gleam.run/modules/html.html)
- [W3Schools HTML-tutorial](https://www.w3schools.com/html/)
- [Mozilla Developer Network HTML-dokumentation](https://developer.mozilla.org/en-US/docs/Web/HTML)