---
title:                "Go: Skrive tester"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av utviklingsprosessen i Go-programmering. Det gir sikkerhet og stabilitet til koden din. Det hjelper også til med å finne og løse feil tidlig, noe som resulterer i et bedre produkt.

## Slik gjør du det

Når du begynner å skrive tester i Go, må du først importere "testing" pakken. Deretter må du skrive tester for hver funksjon du ønsker å teste. Dette er vanligvis gjort ved å opprette en fil med navnet "xxx_test.go" i samme mappe som koden din.

En grunnleggende test vil se slik ut:

```Go
func TestAdd(t *testing.T) {
	result := add(2,3)

	if result != 5 {
		t.Error("Expected result to be 5, got", result)
	}
}
```

I dette eksemplet er "Testing" typen et objekt av typen "testing.T". Vi bruker "Error" metoden for å teste om resultatet er riktig. Hvis testen feiler, vil den skrive ut en feilmelding med informasjon om hva som gikk galt.

Du kan også bruke flere tester for en enkelt funksjon ved hjelp av "TestXxx" prefix. Det er viktig å huske at disse testene vil bli kjørt i en tilfeldig rekkefølge, så det er viktig å ikke ha avhengigheter mellom testene.

## Dykk dypere

Å skrive tester er umulig å unngå hvis du vil skrive pålitelig kode. Det er viktig å håndtere forskjellige scenarier og tilfeller for å sikre at koden din fungerer som den skal. I tillegg kan du bruke "Benchmark" testing for å sammenligne ytelse i forskjellige tilfeller.

En annen viktig ting å huske på er å sørge for god kodelegehet (code coverage) når du skriver tester. Dette refererer til prosentandelen av koden din som er dekket av tester. Høy kodelegehet indikerer god testdekning, noe som betyr at koden din er godt testet.

## Se også

- [Offisiell Go testing dokumentasjon](https://golang.org/pkg/testing/)
- [Testing og kodelegehet i Go](https://blog.golang.org/cover) 
- [En grundig gjennomgang av Go testing](https://medium.com/learning-the-go-programming-language/testing-in-go-part-i-72b0d33e6dd8)