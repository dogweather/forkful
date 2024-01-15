---
title:                "Utskrift av debuggutdata"
html_title:           "Gleam: Utskrift av debuggutdata"
simple_title:         "Utskrift av debuggutdata"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utdata är en användbar metod för att felsöka och förbättra koden. Genom att utskriften ger oss information om vad som händer under körning, kan vi hitta och lösa problem snabbare.

## Hur man gör det

Det finns flera sätt att skriva ut debug-utdata i Gleam, beroende på vilken typ av information vi vill ha.

### Utskrift av variablers värden

För att skriva ut värdet av en variabel kan vi använda ```Gleam.Debug.print```-funktionen. Till exempel om vi vill skriva ut värdet av en variabel ```antal```, kan vi göra det på följande sätt:

```Gleam
let antal = 5

Gleam.Debug.print("Variabelns värde är: $(antal)")
```

Detta kommer att resultera i utskriften "Variabelns värde är: 5". Vi kan också skriva ut flera variabler samtidigt genom att använda flera ```print```-anrop.

### Utskrift av strängar

För att skriva ut en sträng använder vi ```Gleam.Debug.print_string```-funktionen. Om vi till exempel vill skriva ut strängen "Hej!", kan vi göra det så här:

```Gleam
Gleam.Debug.print_string("Hej!")
```

Detta kommer att skriva ut "Hej!" i terminalen.

### Utskrift av typer

Om vi vill skriva ut typen av en variabel, kan vi använda ```Gleam.Debug.print_type```-funktionen. Till exempel om vi vill skriva ut typen av variabeln ```namn```, kan vi göra det med följande kod:

```Gleam
let namn = "Gleam"

Gleam.Debug.print_type("Variabelns typ är: ", namn)
```

Detta kommer att skriva ut "Variabelns typ är: String" i terminalen.

## Utforska djupare

Det finns många fler funktioner för att skriva ut debug-utdata i Gleam, som att skriva ut listor, tupler och liknande. Vi kan också använda ```Gleam.Debug.inspect```-funktionen för att få mer detaljerad information om objekt. Utforska gärna dokumentationen för mer information.

## Se även

- [Officiell Gleam-dokumentation för att skriva ut debug-utdata](https://gleam.run/documentation/guides/printing_debug_output)
- [Gleam-coding examples](https://github.com/gleam-lang/gleam/tree/master/examples)