---
title:                "Å skrive tester"
html_title:           "Go: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Tests er en viktig del av programmering som hjelper utviklere å sikre at koden deres fungerer som den skal. Det er en måte å bekrefte at endringene vi gjør ikke ødelegger det som allerede fungerer, og det hjelper oss å finne og løse feil eller bugs.

## Slik gjør du det:

For å skrive tester i Go, må du inkludere "testing" pakken og skrive testfunksjoner som starter med "Test". Disse funksjonene tar et "testing.T" parameter og bruker "testing" pakken sine metoder som "Error" eller "Fail" for å indikere testresultater. Her er et eksempel for en test av en enkel funksjon som legger sammen to tall:

```Go
func TestAddition(t *testing.T) {
	result := Add(2, 3)
	
	if result != 5 {
		t.Errorf("Expected 5, got %d", result)
	}
}
```
Resultatet av denne testen vil være "PASS" hvis "Add(2, 3)" returnerer 5, ellers vil den være "FAIL" og vise feilmeldingen du har definert.

## I dybden:

Å skrive tester har blitt en vanlig praksis i moderne programmering, og det finnes også andre verktøy og rammer for å skrive tester i Go, for eksempel "go test" kommandolinjeverktøyet og "gomock" for mocking. Det er også vanlig å følge "Test Driven Development" (TDD) metoden, hvor du først skriver en test som feiler, så implementerer du koden som får testen til å passere.

## Se også:

Du kan lære mer om å skrive tester i Go ved å utforske offisiell dokumentasjon for "testing" pakken og bruke ressurser som "Go by Example" for å se flere eksempler. Å lære mer om TDD og andre testingsteknikker kan også hjelpe deg å bli en bedre utvikler. Lykke til!