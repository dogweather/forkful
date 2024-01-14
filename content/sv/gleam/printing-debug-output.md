---
title:    "Gleam: Utskrift av felsökningsutdata"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

När vi kodar, stöter vi ofta på buggar eller felaktigt beteende. Att använda "console.log" är ett vanligt sätt att felsöka, men i Gleam finns det en mer robust lösning - utskrift av debug-output. Detta ger oss en mer strukturerad och läsbar utdata, vilket kan underlätta felsökningen av våra program.

## Hur man gör

För att skriva ut debug-output i Gleam, använder vi funktionen "gleam/diagnostics/print". Denna funktion tar en parameter som är det värde vi vill skriva ut. Vi kan också använda "file_print" från "gleam/diagnostics/file" för att skriva ut till en specifik fil istället för konsolen.

```Gleam
import gleam/diagnostics

let name = "John"

gleam/diagnostics/print(name)
```

Detta kommer att skriva ut värdet av variabeln "name" i konsolen:

```
"John"
```

Vi kan också skriva ut flera värden samtidigt genom att använda funktionen "print_list":

```Gleam
import gleam/diagnostics

let name = "John"
let age = 30

gleam/diagnostics/print_list([name, age])
```

Detta kommer att skriva ut både namnet och åldern i konsolen:

```
"John"
30
```

För att skriva ut till en fil, behöver vi också ange filnamnet:

```Gleam
import gleam/diagnostics
import gleam/diagnostics/file as file

let name = "John"

file_file.write(name, "person.txt")
```

Detta kommer att skriva ut namnet till en fil som heter "person.txt".

## Djupdykning

Att använda debug-output kan vara till stor hjälp när vi behöver felsöka våra program eller förstå hur våra funktioner och variabler interagerar. Det ger oss möjlighet att se värdena på våra variabler och därmed förstå varför ett visst beteende uppstår. Det är också användbart när vi behöver testa en del av koden för att se om det fungerar som förväntat.

Det finns också olika nivåer på debug-output som kan hjälpa oss att fokusera på specifika delar av vårt program. Vi kan använda "debug_print" för att skriva ut information om en viss del av koden, eller "trace_print" för att följa koden genom dess exekvering.

## Se även

- [Gleam Diagnostics modulen](https://gleam.run/modules/gleam/gleam/diagnostics)
- [Gleam File modulen](https://gleam.run/modules/gleam/gleam/diagnostics/file)