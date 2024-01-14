---
title:    "Go: Skriver til standardfeil"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor
Når man utvikler i Go, kommer man ofte over situasjoner hvor man ønsker å skrive ut feilbeskjeder til standard error. Dette kan være nyttig for å feilsøke og forbedre koden din.

## Hvordan gjøre det
For å skrive til standard error, kan man bruke "stderr" til å få tilgang til standard error-strømmen. Dette kan gjøres ved hjelp av "os.Stderr" i "log" pakken. Her er et eksempel på hvordan man kan skrive til standard error:

```Go
package main

import (
    "fmt"
    "os"
    "log"
)

func main() {
    name := "Peter"
    err := errors.New("Invalid input")
    log.New(os.Stderr, "ERROR: ", log.LstdFlags).Printf("Hello %s, there was an error: %v", name, err)
}
```

Resultatet av dette eksempelet vil bli:

```
ERROR: Hello Peter, there was an error: Invalid input
```

Dette viser hvordan man kan bruke "log" pakken til å skrive til standard error. Man kan også bruke den innebygde "fmt" pakken til å skrive til standard error, ved å bruke "Fprintln" funksjonen:

```Go
fmt.Fprintln(os.Stderr, "Oops, something went wrong")
```

Dette vil gi samme resultat som eksempelet ovenfor.

## Deep Dive
For å dykke dypere inn i hvordan man kan skrive til standard error i Go, kan man se på hvordan selve standard error-strømmen fungerer. Standard error er en ut-av-boksen stream som brukes til å skrive ut feilmeldinger og andre viktige meldinger. Denne streamen brukes ofte for å skille feilmeldinger fra vanlige utskrifter som skrives ut til standard out ("stdout").

Man kan også gjøre forskjellige ting med standard error-strømmen, som å lese fra den eller endre fargene på utskriftene. Dette kan gjøres ved å bruke andre pakker som "color" eller "bufio". Med disse pakkene kan man gjøre standard error-utskrifter mer synlige og enklere å lese.

## Se også
- [Go Log Package](https://golang.org/pkg/log/)
- [Go Fmt Package](https://golang.org/pkg/fmt/)
- [Go Color Package](https://github.com/fatih/color)
- [Go Bufio Package](https://golang.org/pkg/bufio/)

Det er alt som trengs for å skrive til standard error i Go. Bruk disse tipsene og triksene for å forbedre feilsøkingsprosessen og skrive bedre kode. Lykke til med kodingen!