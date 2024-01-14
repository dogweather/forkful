---
title:    "Gleam: Läsa kommandoradsargument"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Att kunna läsa kommandoradsargument är en viktig färdighet för alla som programmerar på Gleam. Det är ett sätt att läsa in data och variabler från terminalen och kan vara användbart för att göra din kod mer dynamisk och interaktiv.

## Så här

För att börja läsa kommandoradsargument i Gleam behöver du först importera modulen "sys" som ger tillgång till funktioner för att hantera terminalen.

```
Gleam
import sys
```

För att läsa in ett enkelt argument med en sträng som värde kan du använda funktionen "sys.argv" och ange indexet på argumentet du vill läsa in.

```
Gleam
let förnamn = sys.argv[1]
```

Om du till exempel kör din Gleam-kod från terminalen och skrivit ditt förnamn som argument, kommer värdet av förnamnet att lagras i variabeln "förnamn". För att sedan använda värdet kan du skriva ut det med hjälp av "IO.println".

```
Gleam
IO.println("Hej" ++ förnamn ++ "!")
```

Om inget argument ges kommer funktionen "sys.argv" att returnera en tom lista. Det är också möjligt att läsa in flera argument på en gång och lagra dem i en lista.

```
Gleam
let argument = sys.argv 
IO.println("Du har givit" ++ length(argument) ++ "argument.")
```

I exemplet ovan använder vi funktionen "length" för att räkna antalet argument som har lästs in från terminalen.

## Djupdykning

Förutom att läsa in enkla värden kan du också använda funktionen "sys.argv" för att läsa in kommandoradsflaggor och valfri information från terminalen. Genom att använda funktionen "String.slice" kan du ta bort flaggan och få tillgång till värdet.

```
Gleam
let flagga = "--viktig-flagga"
let index = sys.argv |> index_of(flagga)
let värde = String.slice(start=index + 1, end=2)
```

## Se även

- [Gleams dokumentation om sys-modulen på Engelska](https://gleam.run/articles/command_line_arguments/)
- [En guide för Gleam på Svenska](https://github.com/gleam-lang/gleam/wiki/Hem)
- [Gleams officiella hemsida](https://gleam.run/)