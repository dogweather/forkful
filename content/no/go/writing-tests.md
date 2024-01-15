---
title:                "Skriver tester"
html_title:           "Go: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i koden din kan kanskje virke som tidkrevende og unødvendig arbeid, men det kan faktisk være en verdifull investering på lang sikt. Tester bidrar til å oppdage feil og sikre at koden din fungerer som den skal, og gjør det lettere å vedlikeholde og utvide koden i fremtiden.

## Slik gjør du det

For å skrive tester i Go, må du først importere "testing" pakken. Deretter kan du definere testfunksjoner ved å bruke "Test" prefix før navnet på funksjonen. Inne i testfunksjonen bruker du "t" objektet for å kjøre tester og sjekke verdier ved hjelp av "t.Run" og "t.Fail" metoder. Se et eksempel nedenfor:

```Go
import "testing"

func TestAddition(t *testing.T) {
  result := add(2, 3)
  if result != 5 {
    t.Fail()
  }
}
```

For å kjøre testene dine, bruk kommandoen "go test" i terminalen. Dette vil kjøre alle testfunksjonene i pakken og gi deg en rapport om eventuelle feil som er oppdaget. Du kan også bruke flagg for å kjøre bestemte tester eller få mer detaljert informasjon om resultatene.

## Dypdykk

Å skrive gode tester handler ikke bare om å få dem til å passere, men også om å skrive dem på en effektiv og pålitelig måte. Husk at tester bør være uavhengige, lesbare og dekkende for å gi deg tillit til koden din. Du kan også skrive benchmarks for å sammenligne forskjellige implementasjoner av funksjoner og identifisere eventuelle ytelsesproblemer.

## Se også

- [Testing i Go](https://golang.org/pkg/testing/)
- [God testingpraksis for Go](https://blog.alexellis.io/golang-writing-unit-tests/)