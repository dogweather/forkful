---
title:                "Läsa en textfil"
date:                  2024-01-20T17:54:20.284140-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa en textfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil innebär att man hämtar och använder information lagrad i en fil. Programmerare gör detta för att kunna bearbeta data, ladda konfigurationer eller läsa in extern innehåll i sina applikationer.

## Hur man gör:
I Gleam kan du använda standardbiblioteket för att enkelt läsa från en fil. Här är ett no-nonsense exempel:

```gleam
import gleam/io

pub fn main() {
  let result = io.open("hello.txt") // Öppnar fil
    |> result.map(io.read) // Läser innehållet
    |> result.map(String.from_slice) // Konverterar bytes till sträng
  case result {
    Ok(content) -> io.println(content) // Skriver ut innehållet
    Error(_) -> io.println("Kunde inte läsa filen.")
  }
}
```

## Fördjupning
Historiskt sett härrör filhantering från de tidiga dagarna av operativsystem, med grunden lagd av Unix filsystem och C's standardbibliotek. I Gleam, och dess föregångare Erlang, hanterar vi filer genom ett mönster känd som Actor Model, vilket ger robusta egenskaper för konkurrenshantering. I jämförelse med andra språk, kan alternativ som Node.js använda icke-blockande I/O, medan Python erbjuder synkrona och asynkrona alternativ. Gleam satsar på tydlighet och robusthet i sin implementation, där varje fil-åtgärd returnerar ett "Result" datatyp som måste matchas, vilket ger en säker hantering av potentiella fel.

## Se även
- Gleam's IO module docs: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Erlang's file module (som Gleam lutar sig mot): http://erlang.org/doc/man/file.html
- En guide till Actor Model: https://en.wikipedia.org/wiki/Actor_model
