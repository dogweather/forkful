---
title:                "Sammanfogning av strängar"
html_title:           "Go: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Konkatenation av strängar är när du kombinerar flera strängar till en enda längre sträng. Programmers använder detta för att skapa dynamiska meddelanden, bygga URL-adresser, och för andra tillfällen när olika delar av text behöver sammanfogas till en helhet.

## Hur gör man:
```Go 
// Skapa en variabel som håller den första strängen
första := "Hej"

// Skapa en variabel som håller den andra strängen
andra := "världen!"

// Kombinera strängarna med hjälp av + operatorn
resultat := första + " " andra

// Skriv ut den kombinerade strängen
fmt.Println(resultat)

// Output: Hej världen!
```
Genom att använda + operatorn, kan vi enkelt kombinera strängar och skapa dynamiska meddelanden.

## Djupdykning:
Detta koncept av att sammanfoga eller "konkatenation" av strängar har funnits länge inom programmering, och har funnits i andra språk som C och C++. Andra alternativ för att kombinera strängar i Go inkluderar metoder som `fmt.Sprintf` eller `strings.Join`. När det kommer till att faktiskt implementera detta i koden, så bör man vara medveten om effektiviteten, då varje konkatenation skapar en ny sträng vilket kan påverka prestandan vid hantering av stora mängder data.

## Se även:
- [Go Docs: utf-8 and string operations](https://golang.org/doc/faq#utf_and_string_ops)
- [The Go Programming Language Specification: Operators](https://golang.org/ref/spec#Operators)