---
title:                "För att hitta längden på en sträng"
html_title:           "Go: För att hitta längden på en sträng"
simple_title:         "För att hitta längden på en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Det är ofta användbart att kunna hitta längden på en sträng i sitt kodprojekt för att kunna hantera data på ett effektivt sätt.

## Hur man gör

För att hitta längden på en sträng i Go, kan du använda funktionen `len()` tillsammans med variabeln som innehåller strängen. Här är ett exempel på hur du kan göra det:

```Go
text := "Hej på dig!"
length := len(text)
fmt.Println(length) // Output: 11
```

Funktionen `len()` returnerar antalet tecken i strängen, i detta fall 11. Det är viktigt att notera att funktionen hanterar även specialtecken och diakritiska tecken korrekt.

## Deep Dive

För att förstå hur funktionen `len()` fungerar bakom kulisserna, kan vi titta på kodens interna struktur. I Go är en sträng egentligen en sekvens av tecken som lagras i en byte-array. När du använder `len()` på en sträng, räknar funktionen helt enkelt antalet bytes i den arrayen. Detta förklarar också varför funktionen hanterar specialtecken korrekt, eftersom varje tecken är lagrat som en eller flera bytes.

Det finns också en annan metod för att hitta längden på en sträng i Go, nämligen `unicode/utf8` paketet. Detta paket innehåller funktionen `utf8.RuneCountInString()` som räknar antalet tecken i en sträng. Detta är användbart om du vill hantera flerspråkiga strängar som innehåller tecken från olika alfabet.

## Se även

- [Go Språkspecifikation](https://golang.org/ref/spec)
- [Go Dokumentation](https://golang.org/doc/)