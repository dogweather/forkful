---
title:                "Feilhåndtering"
date:                  2024-01-26T00:53:51.823040-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Feilhåndtering i Go handler om å finne og håndtere utførelsesproblemer på en smidig måte. Vi gjør dette for å forhindre krasjer og sikre at programmene våre fungerer forutsigbart, selv når ting går galt.

## Hvordan gjøre det:

Go bruker eksplisitt feilhåndtering. Det betyr at du må sjekke om en funksjon returnerer en feil hver gang du kaller den. Ingen unntak. Slik ser det ut:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Å nei:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Later som om noe gikk galt
	return fmt.Errorf("noe gikk galt")
}
```

Kjør dette, og du får:

```
Å nei: noe gikk galt
```

Men hva om det lykkes?

```Go
func doSomething() error {
	// Alt bra denne gangen
	return nil
}
```

Ingen utskrift. Kult, ingen nyheter er gode nyheter.

## Dypdykk:

I Go har feilhåndtering vært et omstridt punkt. Fra starten av valgte Go bort fra unntak til fordel for en mer eksplisitt tilnærming, som noen utviklere elsker for dens enkelhet og andre synes er ordrik. Den innebygde `error`-typen er et grensesnitt. Enhver type med en `Error() string`-metode tilfredsstiller det. Dette binder an med Goes etos om enkelhet og eksplisitthet.

Alternativer? Det er duoen `panic` og `recover`, men de er for spesielle tilfeller (ordspill ment) når programmet ikke kan fortsette. Tenk på `panic` som en utkasterknapp du trykker på når du vet at det ikke er noen vei tilbake. Bruk den med måte.

Når det gjelder hovedstrøms feilhåndtering introduserte Go 1.13 feilinnpakning, noe som gjør det enklere å finne ut av "feilkjeden" med funksjoner som `errors.Is()` og `errors.As()`.

## Se også:

For alt som handler om feilhåndtering i Go:

- Go Blog om feilhåndtering: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – seksjon om feilhåndtering: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 dokumentasjon for feilinnpakning: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheneys innlegg om strategier for feilhåndtering: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
