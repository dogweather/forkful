---
title:                "Parsa ett datum från en sträng"
html_title:           "Go: Parsa ett datum från en sträng"
simple_title:         "Parsa ett datum från en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför? 
Att "parsa" ett datum från en sträng innebär att konvertera en textrepresentation av ett datum till ett datatyp som kan hanteras av datorn. Detta är användbart för programmerare eftersom det gör det möjligt att utföra beräkningar eller jämföra datum på ett enkelt sätt.

# Hur man gör: 
```Go
dateString := "3 december 2021"
layout := "2 January 2006" // Anger det specifika datumformatet som behövs
parsedDate, _ := time.Parse(layout, dateString) // Parsar datumet från strängen

fmt.Println(parsedDate) // Skriver ut det formaterade datet som ett objekt av typen time.Time
// Output: 2021-12-03 00:00:00 +0000 UTC
```

# Djupdykning:
Parsering av datum från strängar har varit en nödvändig funktion för programmerare i många år. I äldre språk var det en ganska komplicerad process, men moderna språk som Go har inbyggda funktioner, som time.Parse (), som gör det enkelt och effektivt.

En alternativ metod för att använda date.Parse () är att använda date.ParseExact (), som låter oss specificera det exakta formatet för datumet som ska parsas. Detta kan vara användbart för att hantera olika lokala format eller om strängen innehåller mer än bara ett datum.

# Se även:
Officiell dokumentation för time.Parse () - https://golang.org/pkg/time/#Parse
Video tutorial på YouTube: "Parsar datum i Go" - https://www.youtube.com/watch?v=VyZHxMkzUXo