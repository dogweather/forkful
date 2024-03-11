---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:36.588271-07:00
description: "Strenginterpolasjon er en metode for \xE5 konstruere strenger som inkorporerer\
  \ variabler, noe som muliggj\xF8r dynamisk oppretting av strenger. Programmerere\u2026"
lastmod: '2024-03-11T00:14:13.764373-06:00'
model: gpt-4-0125-preview
summary: "Strenginterpolasjon er en metode for \xE5 konstruere strenger som inkorporerer\
  \ variabler, noe som muliggj\xF8r dynamisk oppretting av strenger. Programmerere\u2026"
title: Interpolering av en streng
---

{{< edit_this_page >}}

## Hva & hvorfor?

Strenginterpolasjon er en metode for å konstruere strenger som inkorporerer variabler, noe som muliggjør dynamisk oppretting av strenger. Programmerere gjør dette for å tilpasse meldinger, konstruere URL-er, lage SQL-forespørsler og mer, noe som tillater mer lesbar og vedlikeholdbar kode.

## Hvordan:

I Go oppnås strenginterpolasjon vanligvis ved bruk av `fmt`-pakken, spesielt med `Sprintf`-funksjonen, som lar deg injisere variabler i en streng ved å spesifisere formatteringsverb. Verbene er plassholdere i formatstrengen og erstattes av de gitte variablenes verdier. Slik bruker du det:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Å bruke Sprintf for strenginterpolasjon
    message := fmt.Sprintf("Hei, jeg heter %s og jeg er %d år gammel.", name, age)
    fmt.Println(message) // Utdata: Hei, jeg heter Jane og jeg er 28 år gammel.
}
```

Merk at `%s` brukes for strenger, og `%d` for heltall. `fmt`-pakkens dokumentasjon gir en omfattende liste over formatteringsverb for forskjellige datatyper.

## Dypdykk

Konseptet med strenginterpolasjon finnes i mange programmeringsspråk, om enn med forskjellige syntakser og kapasiteter. I Go, selv om `fmt`-pakkens `Sprintf`-funksjon er den mest vanlige tilnærmingen, er det kanskje ikke alltid den mest effektive, spesielt for enkle sammenføyninger eller når man arbeider innenfor kode som er svært følsom for ytelse.

`fmt`-pakken bruker refleksjon for dynamisk å tolke variablenes typer ved kjøretid, noe som, selv om det er fleksibelt, medfører overhead. For scenarier hvor ytelse er kritisk, kan direkte strengsammensetning eller `strings.Builder`-typen tilby bedre alternativer. Direkte sammensetning er grei, men kan bli uhandterlig med flere variabler. `strings.Builder`, på den andre siden, gir en mer ytelseseffektiv og lesbar måte å bygge komplekse strenger på i en løkke eller når man håndterer mange variabler:

```go
var sb strings.Builder
sb.WriteString("Hei, jeg heter ")
sb.WriteString(name)
sb.WriteString(" og jeg er ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" år gammel.")
message := sb.String()

fmt.Println(message) // Gir ut det samme som før
```

Til syvende og sist avhenger valget mellom `fmt.Sprintf`, direkte sammensetning og `strings.Builder` av de spesifikke kravene til applikasjonen din, som for eksempel kompleksiteten til strengen som blir konstruert og ytelseshensyn.
