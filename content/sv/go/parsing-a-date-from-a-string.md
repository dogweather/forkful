---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:36:25.123362-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Datumtolkning från en sträng handlar om att omvandla text till en datumtyp din kod kan hantera. Vi gör det för att enkelt kunna jämföra datum, göra tidsberäkningar, eller bara för att normalisera datumformat i våra applikationer.

## How to:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	layout := "2006-01-02 15:04:05" // Go's reference time format
	dateString := "2023-03-14 15:26:00"

	parsedDate, err := time.Parse(layout, dateString)
	if err != nil {
		fmt.Println("Parsing error:", err)
		return
	}

	fmt.Println("Parsed Date:", parsedDate)
}
```
Output:
```
Parsed Date: 2023-03-14 15:26:00 +0000 UTC
```

## Deep Dive
Datumformat i strängar är inte nya. Historiskt har olika kulturer och system använt olika format. Go's `time` paket hanterar datum och tid effektivt. Konstanten för att ange datumformat- `layout`- är baserad på en specifik tidpunkt (referensdatumet): den 2:a januari år 2006, klockan 15:04:05, eftersom den innehåller alla numeriska representationer 1-7 (1 månad, 2 dag, 3 timmar, etc.). Alternativ inkluderar tredjepartsbibliotek som `dateparse` som kan tolka flera datumformat utan att definiera en `layout`. Vissa implementationer använder parsing baserad på tidszon eller språkinställningar, men Go's `time` paket prioriterar en strikt och tydlig layoutdefinition före tolkning.

## See Also
- Go's `time` package documentation: https://golang.org/pkg/time/
- The `dateparse` Go library: https://github.com/araddon/dateparse
- "The Absolute Minimum Every Software Developer Absolutely, Positively Must Know About Unicode and Character Sets": https://www.joelonsoftware.com/2003/10/08/
