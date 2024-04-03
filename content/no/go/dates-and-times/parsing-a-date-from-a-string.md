---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:11.013444-07:00
description: "Hvordan: Go gir robust st\xF8tte for parsing av datoer og tider gjennom\
  \ `time`-pakken. N\xF8kkelen er \xE5 forst\xE5 Go's referansedatoformat: `Mon Jan\
  \ 2 15:04:05 MST\u2026"
lastmod: '2024-03-13T22:44:40.277276-06:00'
model: gpt-4-0125-preview
summary: "Go gir robust st\xF8tte for parsing av datoer og tider gjennom `time`-pakken."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
Go gir robust støtte for parsing av datoer og tider gjennom `time`-pakken. Nøkkelen er å forstå Go's referansedatoformat: `Mon Jan 2 15:04:05 MST 2006`, som du bruker for å fortelle Go hvordan den inngående strengen skal tolkes. Her er et raskt eksempel for å komme i gang:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Eksempel datostreng
	dateStr := "2023-04-12 14:45:00"
	
	// Definer layout/format på inndatodato strengen
	// Dette layoutet forteller Go å forvente et år, etterfulgt av en måned,
	// deretter en dag, time, minutt og til slutt sekund
	layout := "2006-01-02 15:04:05"
	
	// Parse datostrengen i henhold til layoutet
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Feil ved parsing av dato:", err)
		return
	}
	
	// Skriv ut den parsede datoen
	fmt.Println("Parsert Dato:", parsedDate)
}
```

Når du kjører denne koden, vil du få:

```
Parsert Dato: 2023-04-12 14:45:00 +0000 UTC
```

Legg merke til hvordan `layout`-strengen bruker referansedatoens verdier for å spesifisere formatet på inndatastrengen. Juster `layoutet` for å matche formatet til dine inndatodatoer.

## Dypdykk
Designet av Go's dato- og tidsparingsmekanisme er unikt, og benytter seg av en spesifikk referansedato (`Mon Jan 2 15:04:05 MST 2006`). Denne tilnærmingen, i stedet for å bruke mer konvensjonelle format spesifikatorer (som `YYYY` for år), ble valgt for lesbarhet og brukervennlighet, og utnytter et mer eksempelbasert format.

Selv om dette i utgangspunktet kan virke uvanlig for programmere vant til andre språk, finner mange det mer intuitivt etter en kort tilpasningsperiode. For applikasjoner som krever mer kompleks datomanipulasjon eller formater som ikke direkte støttes av Go's `time`-pakke, kan tredjepartsbiblioteker som `github.com/jinzhu/now` tilby ekstra funksjonalitet. Imidlertid, for flertallet av standardapplikasjoner, er Go's innebygde funksjoner robuste, ytelsessterke og idiomatiske, og legemliggjør Go-filosofien om enkelhet og klarhet.
