---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:06.229345-07:00
description: "Hur man g\xF6r: Go erbjuder robust st\xF6d f\xF6r att tolka datum och\
  \ tider genom `time`-paketet. Nyckeln \xE4r att f\xF6rst\xE5 Go:s referensdatumformat:\
  \ `Mon Jan 2\u2026"
lastmod: '2024-03-13T22:44:37.402505-06:00'
model: gpt-4-0125-preview
summary: "Go erbjuder robust st\xF6d f\xF6r att tolka datum och tider genom `time`-paketet."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
Go erbjuder robust stöd för att tolka datum och tider genom `time`-paketet. Nyckeln är att förstå Go:s referensdatumformat: `Mon Jan 2 15:04:05 MST 2006`, som du använder för att berätta för Go hur den inkommande strängen ska tolkas. Här är ett snabbt exempel för att komma igång:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Exempel på datumsträng
	dateStr := "2023-04-12 14:45:00"
	
	// Definiera layout/format på den inkommande datumsträngen
	// Denna layout talar om för Go att förvänta sig ett år, följt av en månad, 
	// sedan en dag, timme, minut och slutligen sekund
	layout := "2006-01-02 15:04:05"
	
	// Tolka datumsträngen enligt layouten
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Fel vid tolkning av datum:", err)
		return
	}
	
	// Skriv ut det tolkade datumet
	fmt.Println("Tolkat Datum:", parsedDate)
}
```

När du kör denna kod får du:

```
Tolkat Datum: 2023-04-12 14:45:00 +0000 UTC
```

Observera hur `layout`-strängen använder referensdatumets värden för att specificera formatet på indatasträngen. Justera `layout` för att matcha formatet på dina indata datum.

## Fördjupning
Designen av Go:s datum- och tidstolkning är unik, och använder ett specifikt referensdatum (`Mon Jan 2 15:04:05 MST 2006`). Detta tillvägagångssätt, istället för att använda mer konventionella formatangivelser (som `YYYY` för år), valdes för läsbarhet och enkelhet, och utnyttjar ett mer exempelbaserat format.

Även om detta initialt kan verka ovanligt för programmerare vana vid andra språk, finner många det mer intuitivt efter en kort justeringsperiod. För applikationer som kräver mer komplex datummanipulation eller format som inte direkt stöds av Go:s `time`-paket, kan tredjepartsbibliotek som `github.com/jinzhu/now` erbjuda ytterligare funktionalitet. Dock, för majoriteten av standardapplikationer, är Go:s inbyggda funktioner robusta, prestandaeffektiva och idiomatiska, och förkroppsligar Go-filosofin om enkelhet och klarhet.
