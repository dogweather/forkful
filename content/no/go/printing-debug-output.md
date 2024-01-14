---
title:                "Go: Utskrift av feilsøkingstekst"
simple_title:         "Utskrift av feilsøkingstekst"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut feilsøkingsutdata er en viktig del av å skrive effektiv Go-kode. Det kan hjelpe deg med å identifisere og løse feil i koden din, og gjøre det enklere å forstå hva som skjer under kjøretiden. I denne bloggposten vil vi se på hvorfor det er viktig å skrive ut debug utdata, og hvordan du kan gjøre det på en enkel måte.

## Hvordan

For å skrive ut debug utdata i Go, kan du bruke funksjonen ```fmt.Println()```. Denne funksjonen tar inn hvilken som helst type data som argument og skriver den ut til standard utgang, som vanligvis er terminalvinduet. La oss se på et eksempel:

```
package main

import "fmt"

func main() {
  num := 42
  txt := "Hello World!"
  fmt.Println("Nummer:", num)
  fmt.Println("Tekst:", txt)
}
```

I dette eksemplet bruker vi ```fmt.Println()``` for å skrive ut verdien av to variabler, ```num``` og ```txt```. Output fra dette programmet vil være:

```
Nummer: 42
Tekst: Hello World!
```

Som du kan se, skriver funksjonen ut en tekststreng fulgt av verdien av variabelen. Dette kan være nyttig for å verifisere at verdiene du forventer er riktig, eller for å få en oversikt over hvordan dataene dine endrer seg gjennom koden din.

## Deep Dive

For mer avansert debugging, kan du også bruke ```fmt.Printf()``` som lar deg formatere utdataen din. Denne funksjonen bruker vanligvis en formatteringsstreng, der forskjellige symboler brukes til å representere ulike typer data, som for eksempel tall, tekst eller boolske verdier. Her er et eksempel på hvordan du kan bruke det:

```
package main

import "fmt"

func main() {
  fruit := "apple"
  num1 := 20
  num2 := 3
  fmt.Printf("Jeg liker å spise %v(er), det gir meg %d energi hver dag og gjør meg %v\n", fruit, num1, (num1*num2 > 50))
}
```

I dette eksemplet bruker vi forskjellige symboler som ```%v``` for å representere en generell verdi, ```%d``` for tall, og ```%v``` for å representere en boolsk verdi. Output fra dette programmet vil være:

```
Jeg liker å spise apple(r), det gir meg 20 energi hver dag og gjør meg true
```

Dette er bare et enkelt eksempel, men du kan bruke en rekke forskjellige symboler og formateringsalternativer for å få utdataen din akkurat slik du vil ha den.

## Se Også

- [Offisiell dokumentasjon for fmt pakken](https://golang.org/pkg/fmt/)
- [En komplett guide til debugging i Go](https://medium.com/@kdnotes/debugging-in-go-complete-how-to-guide-9a522123f11)
- [Bruk av fmt for å forbedre kodekvalitet i Go](https://levelup.gitconnected.com/using-fmt-to-improve-code-quality-in-go-90cbbf03522c)

Alt i alt kan debugging og utskrift av debug utdata være en avgjørende del av å skrive effektiv og feilfri Go-kode. Ved å bruke ```fmt```-funksjonene riktig, kan du gjøre feilsøking enklere og mer effektivt. Lykke til med å implementere disse teknikkene i dine egne prosjekter!