---
title:                "Sammenkobling av strenger"
html_title:           "C: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger, også kalt konkatenasjon, er en viktig del av programmering uansett språk. Det lar deg dynamisk kombinere ulike tekstlige elementer for å lage mer komplekse og nyttige uttrykk. I C kan det være spesielt nyttig for utskrift av tekst og lagring av informasjon.

## Slik gjør du det

For å slå sammen to strenger i C, bruker du funksjonen `strcat()`. Denne funksjonen tar to strenger som parametere og returnerer en kombinert streng som en ny strengvariabel.

```C
#include <stdio.h>
#include <string.h>

int main() {
	char navn[] = "Ola";
	char etternavn[] = "Nordmann";
	
	// Kombiner navn og etternavn ved hjelp av strcat()
	strcat(navn, etternavn);
	
	// Skriv ut den nye strengen
	printf("Hei, %s!", navn);
	
	// Output: Hei, OlaNordmann!
	
	return 0;
}
```

Som du kan se i eksempelet ovenfor, er det viktig å være oppmerksom på størrelsen til strengene når du slår dem sammen. `strcat()` fungerer ved å finne slutten av den første strengen og deretter kopiere den andre strengen etter den. Derfor er det viktig å sørge for at den første strengen har plass til å lagre den andre strengen i seg.

Hvis du vil legge til flere enn to strenger, kan du bruke denne funksjonen flere ganger etter hverandre. Du kan også bruke `strncpy()` for å begrense lengden på den kombinerte strengen, hvis du ønsker det.

## Gå dypere

Når du bruker `strcat()`, bør du være klar over at den er relativt ineffektiv. Hver gang den blir kalt, må den lete gjennom den første strengen for å finne slutten før den kan kombinere den andre strengen. Dette kan føre til en del unødvendig overhead når du skal kombinere flere strenger.

En annen viktig ting å være oppmerksom på er at `strcat()` ikke sjekker for plass i strengen før den kopierer den andre strengen. Dette kan føre til at data overskrives eller at programmet krasjer hvis ikke strengen er stor nok.

For en mer effektiv og sikker måte å kombinere strenger på, kan du vurdere å bruke funksjonen `snprintf()`. Denne funksjonen tar en mål-streng, en maksimum lengde, og en kombinasjon av strenger og variabler som parametere. Den unngår overfylte strenger og unødvendig søking etter slutten av den første strengen.

## Se også

- [string.h library i C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C String funksjoner tutorial](https://www.learn-c.org/en/Strings)
- [snprintf() funksjonen i C](https://www.geeksforgeeks.org/snprintf-c-library/)