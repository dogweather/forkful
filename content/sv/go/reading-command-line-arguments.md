---
title:                "Läsning av kommandoradsargument"
html_title:           "Go: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Läsning av kommandoradsargument är en vanlig del av programmering, där utvecklare läser in värden som skickas från kommandoraden vid körning av ett program. Detta gör det möjligt att anpassa programmets beteende genom att ge variabler och andra inmatningar vid körning.

## Så här gör du:

Go har inbyggda funktioner för att läsa in kommandoradsargument i ditt program. Här är ett exempel på hur du kan använda dessa funktioner:

```Go 
func main() {
    arguments := os.Args
    fmt.Println(arguments)
}
```

Om vi kör programmet med några argument, till exempel "go run program.go argument1 argument2", kommer utmatningen att vara: [program.go argument1 argument2].

Du kan också använda flag-paketet för att läsa specifika argument med namngivna flaggor. Här är ett exempel:

```Go
package main

import (
    "flag"
    "fmt"
)

func main() {
    var arg1 string
    flag.StringVar(&arg1, "arg1", "default", "This is the first argument")
    var arg2 int
    flag.IntVar(&arg2, "arg2", 5, "This is the second argument")
    flag.Parse()

    fmt.Println("Argument 1:", arg1)
    fmt.Println("Argument 2:", arg2)
}
```

Om vi kör programmet med flaggan "-arg1 hello -arg2 10" som argument, kommer utmatningen att vara:

```Go
Argument 1: hello
Argument 2: 10
```

## Djupdykning:

Att läsa kommandoradsargument har funnits sedan tidigt i programmeringens historia och är en viktig del av många programmeringsspråk, inklusive Go. Andra alternativ för att läsa in kommandoradsargument inkluderar att använda argumentvariabler eller att läsa från standardinput.

I Go läses kommandoradsargumenten som en skivoperator som använder variabeln ```os.Args```, som är en slice av strängar som representerar varje argument som skickats från kommandoraden. För att läsa specifika argument med flaggor används flag-paketet, som ger en enkel möjlighet att definiera och läsa flaggor och deras värden.

## Se även:

- [Officiell dokumentation för os.Args](https://golang.org/pkg/os/#pkg-variables)
- [Officiell dokumentation för flag-paketet](https://golang.org/pkg/flag/)