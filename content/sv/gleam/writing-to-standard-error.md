---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standard error (stderr) innebär att skicka error-meddelanden till en separat utström, vilket gör det lättare att skilja dem från vanlig output. Programmers gör detta för att förenkla felsökning och hantering av loggdata.

## Så här gör du:
I Gleam kan du skriva till stderr genom att använda Erlang-bibliotekets funktioner. Här är ett exempel:

```gleam
import gleam/io
import gleam/erlang

pub fn main() {
  io.println("Detta är standard output.")
  erlang.error("Detta är standard error.")
}
```
Kör koden ovan och du kommer att se:

```
Detta är standard output.
Detta är standard error.
```
Notera att "Detta är standard error." kommer att visas i stderr-strömmen.

## Djupdykning:
Historiskt har separationen av standard output och standard error gjorts för att tillåta användare och andra program att skilja normala meddelanden från fel och varningar. Alternativ till att skriva direkt till stderr inkluderar loggbibliotek som kan hantera och dirigera loggar mer sofistikerat. En viktig implementationdetalj i Unix-liknande system är att stderr har filbeskrivare 2.

## Se även:
- Gleam's officiella dokumentation: [https://gleam.run/](https://gleam.run/)
- Bakgrundsinformation om standardströmmar: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)
