---
title:                "Gleam: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument kan vara en viktig del av att skapa effektiva och användbara Gleam-program. I denna bloggpost kommer vi att utforska hur man på ett enkelt sätt kan integrera kommandoradsargument i sina Gleam-program.

## Så här gör du

För att läsa in kommandoradsargument i Gleam använder vi funktionen `gleam/io.CommandLine.parse_args()`. Denna funktion tar emot en lista med strängar (kommandoradsargumenten) och returnerar en lista med tupler innehållande både namnet på argumentet och dess värde.

```Gleam
list = ['-n', '5', '-s', 'Hello']
options = io.CommandLine.parse_args(list)
```

I exemplet ovan kommer `options` att innehålla `[{name: "-n", value: "5"}, {name: "-s", value: "Hello"}]`. Vi kan sedan använda oss av dessa värden i vårt program, till exempel för att sätta antalet iterationer eller hälsningsmeddelandet.

## Djupdykning

För de som är mer intresserade av hur kommandoradsargument fungerar i Gleam, så kan vi titta lite närmare på funktionen `gleam/io.CommandLine.parse_args()`. Denna funktion tar emot en variabel av typen `list(string)` och returnerar en variabel av typen `list({name: string, value: string})`. Den loopar igenom listan av strängar och tittar på varje enskilt argument. Om argumentet börjar med ett `-` så betraktas det som ett namn på ett kommandoradsargument, och värdet som följer kommer att vara värdet för detta argument. Om argumentet inte börjar med ett `-` betraktas det som ett värde till det föregående kommandoradsargumentet.

Vi kan också använda `gleam/io.CommandLine.has_option()` för att enkelt kolla om ett visst argument finns med i listan eller inte.

## Se även

- Gleam-dokumentation för `gleam/io.CommandLine`: [https://gleam.run/documentation/standard_library#gleamioCommandLine](https://gleam.run/documentation/standard_library#gleamioCommandLine) 
- Enkel introduktion till command line arguments på YouTube: [https://www.youtube.com/watch?v=8zXVELs8kL4](https://www.youtube.com/watch?v=8zXVELs8kL4)