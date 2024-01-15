---
title:                "Skrivande till standardfel"
html_title:           "Gleam: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är användbart när vi behöver skriva ut felmeddelanden eller annan viktig information som inte bör visas till användarna. Det är ett enkelt sätt att spåra och felsöka problem i vår kod.

## Hur man gör det

För att skriva till standard error i Gleam, kan vi använda det inbyggda `gleam/errno` biblioteket. Vi importerar det genom att lägga till `import gleam/errno` i början av vår `.gleam` fil.

För att skriva ett felmeddelande till standard error, använder vi funktionen `write`. Vi behöver ge den tre parametrar: ett felmeddelande, en kod för feltyp och en filnamn för att identifiera var felet uppstod.

```Gleam
import gleam/errno

fn write_to_error() {
  write("Oh no, something went wrong!", 1, "my_file")
}
```

Den första parametern är vårt felmeddelande, den andra är vår felkod `1` (vilket indikerar att det är ett generiskt fel) och den tredje är filnamnet `my_file`.

När vi sedan kör vårt program i terminalen, kommer felmeddelandet att skrivas ut till standard error istället för standard output. Vi kan se det genom att använda kommandot `gleam run` följt av vårt filnamn.

```terminal
gleam run my_file.gleam
Oh no, something went wrong!
```

## Djupdykning

Att skriva till standard error är enkelt och användbart, men det finns några saker att tänka på. För det första, är det viktigt att välja en lämplig felkod för att tydligt identifiera felet. I vårt exempel använde vi en generisk felkod `1`, men det finns många andra typer att välja mellan beroende på vilken typ av fel vi stöter på.

En annan sak att tänka på är att vi kan använda `writeln` funktionen istället för `write` för att lägga till en radbrytning efter vårt felmeddelande. Det kan göra det enklare att läsa felmeddelandet i terminalen.

## Se också

* [Gleam - The Erlang VM language made for type safe systems programming](https://gleam.run/)
* [Gleam Standard Library - Sneaky throwing an error with gleam_errno](https://github.com/gleam-lang/gleam_stdlib/blob/master/core/gleam_errno.gleam)
* [Erlang Documentation - Error Handling](http://erlang.org/course/error_handling.html)