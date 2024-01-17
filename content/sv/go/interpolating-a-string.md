---
title:                "Interpolering av en sträng"
html_title:           "Go: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolering av strängar är en metod som används inom programmering för att bygga en sträng med text och variabler. Det är ett effektivt sätt att sammanfoga olika delar till en dynamisk sträng, vilket är användbart för att skapa mer flexibla och anpassningsbara program.

## Så här:

Go har inbyggda funktioner för att interpolera strängar, det vill säga skapa en sträng genom att sammanfoga text och variabler. Detta görs med hjälp av en punkt och ett likamedstecken inuti en sträng, följt av variabelnamnet som ska interpoleras inom måsvingar. Se nedan för ett exempel på hur man kan använda interpolering i Go:

```Go
name := "Lisa"
age := 25
fmt.Printf("Hej, mitt namn är %s och jag är %d år gammal.", name, age)
```
Output:
`Hej, mitt namn är Lisa och jag är 25 år gammal.`

Det går även att interpolera flera variabler i en och samma sträng genom att lägga till fler %tecken och tillhörande variabler inom måsvingar. Se det här exemplet:

```Go
title := "Go programming"
year := 2009
fmt.Printf("%s skapades år %d.", title, year)
```
Output:
`Go programming skapades år 2009.`

## Djupdykning:

Interpolering av strängar har funnits sedan början av 70-talet och används i många programmeringsspråk, även om syntaxen kan skilja sig åt. Ett alternativ till att interpolera strängar är att använda strängformatering, där variabler placeras i en sträng enligt ett särskilt mönster (som t.ex. %s och %t i Go). Båda metoderna har sina egna fördelar och nackdelar, men interpolering av strängar anses ofta vara enklare och mer lättläst.

I Go så byggs själva interpoleringsfunktionen upp av flera mindre delar, bland annat genom användning av buffer-objekt och Formatter-interface. Detta gör att Go kan hantera och interpolera olika typer av variabler, även komplexa datatyper som structs och arrays.

## Se även:

- [Go Language Specification - String Interpolation](https://golang.org/ref/spec#String_interpretation)
- [String Formatting in Go](https://www.digitalocean.com/community/tutorials/how-to-use-string-formatters-in-go)
- [Go Template Package documentation](https://golang.org/pkg/text/template/)