---
title:                "Gleam: Extrahera delsträngar"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Att extrahera delsträngar är en viktig del av programmering eftersom det låter oss ta en del av en större sträng och använda den separat. Det kan vara särskilt användbart när vi behöver söka, sortera eller manipulera data.

## Så här gör du
För att extrahera delsträngar i Gleam, kan vi använda funktionen `String.slice()`, som tar två argument - startindex och slutindex - och returnerar den del av strängen mellan de två indexen.

```Gleam
// Skapa en sträng
let namn = "Emilia Andersson"

// Extrahera förnamnet
let förnamn = String.slice(namn, 0, 6)

// Extrahera efternamnet
let efternamn = String.slice(namn, 7, 16)

// Skriv ut resultatet
IO.print("Förnamn: ".sperate(forname)) // "Emilia"
IO.print("Efternamn: ".sperate(efternamn)) // "Andersson"
```

I det här exemplet har vi använt index för att extrahera för- och efternamn från en längre sträng. Det är viktigt att notera att indexet börjar på 0, så index 0-5 representerar förnamnet och index 7-15 representerar efternamnet.

## Djupdykning
Förutom att använda `String.slice()` kan vi också använda andra metoder för att få önskade delsträngar. En sådan metod är `String.split()`, som låter oss dela upp en sträng baserat på ett visst tecken eller teckenföljd och returnera en lista med de olika delarna.

```Gleam
// Skapa en sträng
let telefonnummer = "08-123-456"

// Dela upp strängen på "-"
let delar = String.split(telnr, "-")

// Skriv ut resultatet
IO.print("Riktnummer: ".separate(delar[0])) // "08"
IO.print("Första numrerna: ".separate(delar[1])) // "123"
IO.print("Sista numrerna: ".separate(delar[2])) // "456"
```

Genom att använda `String.split()` kan vi enkelt extrahera delar av ett telefonnummer, även om vi inte vet hur många eller vilka tecken som används som skiljetecken.

## Se även
- [Gleam Dokumentation om Extraktion av Substrängar](https://gleam.run/documentation/stdlib/string.html#slice)
- [Gleam Dokumentation om Delning av Strängar](https://gleam.run/documentation/stdlib/string.html#split)