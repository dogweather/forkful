---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få det nuvarande datumet handlar om att ta information om intervallet från starten av en tidräkningsperiod till nu. Programmerare gör det för tidstämpelloggar, beräkna deadlines och hantera andra tidrelaterade funktioner.

## Så här gör du:
För att få det nuvarande datumet, använd biblioteket `time` i Go:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()

	fmt.Println("Nuvarande Datum: ", currentTime.Format("2006-01-02"))
}
```
Denna kod ger följande utdata:
```
Nuvarande Datum: 2022-03-01
```
`time.Now()` returnerar nuvarande datum och tid.

## Djupdykning
`time` paketet introducerades i Go 1.0, vilket släpptes år 2012. Det har sina rötter i Unix-tid, som räknar sekunder sedan starten av Unix-era (1 januari 1970).

Alternativen till `time` paketet inkluderar externa bibliotek som `jinzhu/now` för mer komplexa datum- och tidsoperationer.

Implementationen av `time.Now()` i Go returnerar en `Time` typ som innehåller information om nuvarande tidpunkt, bland annat sekunder och nanosekunder sedan Unix-era.

## Se också
1. Officiell Go dokumentation för `time` paketet: https://golang.org/pkg/time/
2. `jinzhu/now` bibliotek: https://github.com/jinzhu/now
3. Unix Tid Wikipedia: https://sv.wikipedia.org/wiki/Unixtid