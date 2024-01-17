---
title:                "Mellanrum html"
html_title:           "Gleam: Mellanrum html"
simple_title:         "Mellanrum html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing av HTML är en process där man bryter ner HTML-kod till dess olika beståndsdelar för att kunna extrahera information från den. Detta är en viktig funktion för programmerare eftersom det möjliggör för oss att skapa webbapplikationer och andra verktyg som kräver åtkomst till specifik data från en webbsida.

## Så här gör man:
```Gleam
let html = "<html><head><title>Min hemsida</title></head><body><h1>Välkommen!</h1></body></html>"

let document = html |> gleam_html_parser.parse

document.title // Returnerar "Min hemsida"
document.body.children[0].name // Returnerar "h1"
document.body.children[0].text // Returnerar "Välkommen!"
```

## Djupdykning:
Parsing av HTML är en nödvändig process för att kunna skapa webbapplikationer och verktyg som behöver använda information från webbsidor. Historiskt sett har språket XML använts för att beskriva strukturen i HTML-dokument, men idag är HTML5 det vanligaste formatet. Det finns även alternativa lösningar för att parsea HTML, men Gleams parser är ett populärt val bland programmerare på grund av dess enkelhet och effektivitet.

## Se även:
- [Gleam HTML parser dokumentation](https://gleam.run/modules/gleam_html_parser/latest/)
- [Gleam HTML parser GitHub repo](https://github.com/gleam-lang/gleam_html_parser)
- [En artikel om parsing av HTML i Gleam](https://medium.com/@bjzaba/writing-a-html-parser-for-the-web-in-gleam-2339bd5c6605)