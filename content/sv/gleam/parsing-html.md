---
title:                "Tolka HTML"
date:                  2024-01-20T15:31:32.906318-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML betyder att analysera och förstå HTML-kod för att utvinna specifik data eller struktur. Programmerare gör detta för att interagera med webbsidor, extrahera information eller automatisera webbuppgifter.

## Hur gör man?:
```Gleam
extern crate gleam_html;
import gleam_html.{parse, Element, Text, Document};

fn main() {
    let html = "<html><body><p>Hello, Gleam!</p></body></html>";
    let document = parse(html);
    
    match document {
        Ok(Document(elements)) -> {
            for element in elements {
                analyse_element(element);
            }
        }
        Error(_) -> {
            io.println("Failed to parse HTML");
        }
    }
}

fn analyse_element(element: Element) {
    match element {
        Element(_, children) -> {
            for child in children {
                match child {
                    Text(text_content) -> io.println(text_content),
                    _ -> {}
                }
            }
        }
        _ -> {}
    }
}
```
Exempelutskrift: `Hello, Gleam!`

## Djupdykning:
Tolkning av HTML är inte unikt för Gleam; det är ett vanligt behov i många programmeringsspråk. Historiskt har bibliotek som Beautiful Soup för Python och Nokogiri för Ruby varit populära för HTML-parsing. Gleam, som är ett relativt nytt och typsäkert funktionellt programmeringsspråk som bygger på Erlang's virtuella maskin, erbjuder sina egna verktyg och fördelar som robust felhantering. Det skiljer sig genom att vara kompilerat istället för tolkat, vilket kan ge prestandafördelar och bättre typsäkerhet i parsingprocessen.

## Se även:
- Gleam's officiella dokumentation: [Gleam Docs](https://gleam.run/documentation/)
- Erlang's officiella webbplats: [Erlang.org](https://www.erlang.org/)