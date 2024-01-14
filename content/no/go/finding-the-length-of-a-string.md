---
title:                "Go: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng er en grunnleggende operasjon som er nødvendig i mange programmeringsscenarier. Enten du må validere brukerinput, formatere tekst eller behandle data, er å kjenne lengden til en streng en viktig del av prosessen.

## Hvordan

For å finne lengden til en streng i Go, kan du bruke den innebygde `len()` funksjonen. Denne funksjonen tar inn en streng og returnerer antall tegn i strengen, inkludert mellomrom og spesialtegn.

```Go
// Definer en streng som skal måles
tekst := "Hei, dette er en test"

// Bruk len() funksjonen til å finne lengden
lengde := len(tekst)

// Printer ut resultatet
fmt.Println(lengde)

// Output: 22
```

I dette eksempelet har strengen 22 tegn, noe som inkluderer mellomrommet mellom `test` og `Hei,`. Du kan også bruke `len()` funksjonen på en tom streng som vil returnere 0.

```Go
tomStreng := ""

lengde := len(tomStreng)

fmt.Println(lengde)

// Output: 0
```

## Dypdykk

En ting å merke seg er at `len()` funksjonen returnerer antall bytes i strengen, ikke antall tegn. Dette kan være viktig å huske på når du jobber med flerspråklige applikasjoner. Når det kommer til å telle lengden på en streng med multibyte tegn, bør du bruke `utf8.RuneCountInString()` funksjonen for å få den nøyaktige lengden.

De fleste strenger i Go er også immutable, noe som betyr at du ikke kan endre på dem etter du har definert dem. Derfor, selv om du endrer på en streng ved å legge til eller fjerne tegn, vil `len()` funksjonen alltid returnere lengden av den originale strengen.

## Se også

- [Go offisiell dokumentasjon for `len()`](https://golang.org/pkg/builtin/#len)
- [Go offisiell dokumentasjon for `utf8.RuneCountInString()`](https://golang.org/pkg/unicode/utf8/#RuneCountInString)
- [TutorialsPoint tutorial om strenger i Go](https://www.tutorialspoint.com/go/go_string_manipulation.htm)