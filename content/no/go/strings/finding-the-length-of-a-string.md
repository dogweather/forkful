---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:50.780530-07:00
description: "\xC5 finne lengden p\xE5 en streng i Go handler om \xE5 bestemme antall\
  \ tegn den inneholder. Programmerere utf\xF8rer rutinemessig denne operasjonen for\
  \ \xE5 manipulere\u2026"
lastmod: '2024-03-13T22:44:40.256981-06:00'
model: gpt-4-0125-preview
summary: "\xC5 finne lengden p\xE5 en streng i Go handler om \xE5 bestemme antall\
  \ tegn den inneholder. Programmerere utf\xF8rer rutinemessig denne operasjonen for\
  \ \xE5 manipulere\u2026"
title: "\xC5 finne lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng i Go handler om å bestemme antall tegn den inneholder. Programmerere utfører rutinemessig denne operasjonen for å manipulere strenger effektivt, enten det er for validering, utdrag av understrenger, eller rett og slett for å håndheve begrensninger i brukerinndata.

## Hvordan gjøre det:
I Go behandles strenger som uforanderlige sekvenser av bytes. Du kan finne lengden på en streng ved hjelp av den innebygde `len()`-funksjonen som returnerer antall bytes, ikke nødvendigvis antall tegn. Slik bruker du den:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Bruker len() for å finne byte-lengden
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Byte Lengde:", byteLength) // Utdata: Byte Lengde: 13

	// For å nøyaktig få antall tegn eller runer i en streng
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Rune Lengde:", runeLength) // Utdata: Rune Lengde: 9
}
```
Den første metoden ved bruk av `len()` gir ikke alltid det forventede resultatet siden den teller bytes. For strenger som inneholder ikke-ASCII-tegn (som "世界"), bør `RuneCountInString` fra `unicode/utf8`-pakken brukes i stedet for å nøyaktig telle Unicode-kodepunkter.

## Dypdykk
Før Go 1 var det ingen streng demarkasjon for behandling av strenger som sekvenser av bytes versus sekvenser av tegn. Etter Go 1, med adopsjonen av UTF-8 som standard tegnkodingsordning for strenger, ble det nødvendig med klarere tilnærminger. `len()`-funksjonen fungerer perfekt for ASCII-strenger, der tegn er representert i en enkel byte. Imidlertid, ettersom Go-applikasjoner ble mer globale, og behovet for å støtte en mengde språk og tegnsett vokste, viste den enkle tilnærmingen av `len()` begrensninger.

Introduksjonen og bruk av `utf8.RuneCountInString()` svarer på disse begrensningene ved å tilby en måte å telle faktiske Unicode-tegn (runer i Go-terminologi) på. Denne metoden sikrer at lengdeberegningen er uavhengig av kodningsspesifikkene til UTF-8, der tegn kan spenne over flere bytes.

Et alternativt tilnærming for å traversere og manipulere strenger, mer i tråd med Go's etos for samtidighet og effektivitet, kan innebære å behandle strenger som skiver av runer. Imidlertid krever denne metoden et konverteringstrinn og løser ikke øyeblikkelig alle finessene ved Unicode (f.eks. kombinerende tegn).

Oppsummert, mens `len()` er egnet for byte-lengde og er effektiv for ASCII-tekst, er `utf8.RuneCountInString()` et mer pålitelig valg for en globalt kompatibel applikasjon. Likevel oppfordres utviklere til å forstå avveiningene i ytelse og minnebruk som disse valgene medfører.
