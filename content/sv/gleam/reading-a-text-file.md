---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil är en grundläggande operation som innebär att inhämta och tolka innehållet i en fil som text. Programmers gör detta för att behandla databaser, konfigurationsfiler eller till och med användarinmatning.

## Hur gör man:

För att läsa en textfil i Gleam kan du använda `file.read`-funktionen. Här är ett enkelt exempel:

```Gleam
import gleam/file.{read}

fn main(args: List(String)) {
  let _ = 
    args
    |> list.head
    |> result.then(read)
    |> result.map(io.println)
    |> result.unwrap(or_else: fn(_) { error("No file was provided") })
} 
```

När du kör detta program med en fil som argument kommer det att skriva ut innehållet i den filen till terminalen.

## Djupdykning

Att läsa textfiler har varit en central del av programmering sedan databehandlingens födelse. Gleam använder en funktionell tillvägagångssätt för att läsa filer, vilket innebär att filen läses in i minnet som ett helt, i stället för att läsas rad för rad som i vissa andra språk.

Ett alternativ till att läsa hela filen åt gången är att använda strömbehandling, där filen läses rad för rad eller bit bit, vilket kan vara effektivare för stora filer.

Gleam's `file.read` är implementerad genom Erlang's inbyggda `file.read_file/1` funktion, vilket betyder att den är både snabb och pålitlig.

## Se också

1. Gleam dokumentation om fil IO: https://gleam.run/book/tour/file-system.html
2. Erlang's `file.read_file`: https://erlang.org/doc/man/file.html#read_file-1
3. Strömbehandling i Gleam: https://gleam.run/book/tour/streams.html