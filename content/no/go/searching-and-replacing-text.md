---
title:                "Go: Søking og utskifting av tekst"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å søke og erstatte tekst er en vanlig oppgave i programmering, spesielt i store prosjekter. Det kan spare deg for mye tid og frustrasjon ved å gjøre det manuelt, og sikre en konsistent kodingstil. Heldigvis har Go programmeringsspråket innebygde funksjoner som gjør det lett å søke og erstatte tekst i en fil eller streng.

# Slik gjør du det

For å søke og erstatte tekst i Go, bruker vi "strings" pakken og dens "Replace" metode. Denne metoden tar inn tre parametere: original tekst, tekst som skal erstattes og erstatningstekst. La oss se på et eksempel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Hei verden!"
	nyTekst := strings.Replace(text, "Hei", "Hallo", 1)
	fmt.Println(nyTekst)
}
```

I dette eksempelet erstatter vi "Hei" med "Hallo" i tekststrengen "Hei verden!". Det tredje argumentet, "1", indikerer hvor mange ganger vi vil gjøre erstatningen. I vårt tilfelle erstattes bare den første forekomsten av "Hei".

Output av dette programmet ville være "Hallo verden!". Hvis vi endrer det tredje argumentet til "2", ville output bli "Hallo verden!", siden det erstatter to forekomster av "Hei".

# Dypdykk

"Replace" metoden er nyttig for de fleste søke- og erstatningsoppgaver, men det er andre funksjoner som kan være mer passende for spesifikke scenarier.

For eksempel, hvis du trenger å gjøre en global erstatning, kan du bruke "ReplaceAll" metoden i stedet. Denne metoden erstatter alle forekomster av den opprinnelige teksten i stedet for å begrense det til et bestemt antall. Det er også "ReplaceAllLiteral" metoden som erstatter tekst uten å tolke eventuelle spesielle karakterer som kan være til stede.

Et annet alternativ er å bruke "Regexp" pakken, som lar deg bruke regulære uttrykk for å søke og erstatte tekst. Dette kan være mer komplekst, men også mer fleksibelt når det gjelder ulike typer søk og erstatning.

# Se også

- [Go strings.Replace](https://golang.org/pkg/strings/#Replace)
- [Go strings.ReplaceAll](https://golang.org/pkg/strings/#ReplaceAll)
- [Go strings.ReplaceAllLiteral](https://golang.org/pkg/strings/#ReplaceAllLiteral)
- [Go Regexp](https://golang.org/pkg/regexp/)