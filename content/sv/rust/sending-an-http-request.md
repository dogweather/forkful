---
title:                "Skicka en http-begäran"
html_title:           "Rust: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi som programmerare pratar om att "skicka ett HTTP-anrop" innebär det att vi skickar en förfrågan till en annan dator eller server för att hämta eller skicka information. Detta är en viktig del av många moderna applikationer och webbtjänster.

## Så här:
För att skicka ett HTTP-anrop i Rust använder vi oss av standardbiblioteket "reqwest". Nedan följer ett exempel på hur man kan skicka en GET-förfrågan och få tillbaka ett svar från en uppsatt server:

```Rust
use reqwest;

let response = reqwest::get("https://www.example.com")?
    .text()?;
```

Detta kommer att skicka en GET-förfrågan till URL:en "https://www.example.com" och lagra svaret som en textsträng i variabeln "response".

## Djupdykning:
HTTP har funnits sedan 1991 och används idag som det primära protokollet för kommunikation på webben. Det finns även andra alternativ, som HTTPS för säker kommunikation och REST för att skicka data till och från en webbtjänst.

Implementationen av HTTP-anrop i Rust med hjälp av "reqwest" ger oss möjlighet att skicka både synkrona och asynkrona förfrågningar, samt hantera olika typer av svar från servern.

## Se även:
- Officiell dokumentation för "reqwest": https://docs.rs/reqwest/
- En introduktion till HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview