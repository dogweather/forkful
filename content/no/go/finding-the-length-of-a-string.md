---
title:    "Go: Å finne lengden av en streng"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I Go-programmering trenger vi ofte å håndtere strenger, enten det er å lagre brukerinput eller behandle tekstlige data. En viktig del av dette er å kunne finne lengden på en streng, det vil si antall tegn den inneholder. Derfor er det viktig å lære hvordan man kan finne lengden til en streng i Go.

## Slik gjør du det

Det er flere forskjellige måter å finne lengden på en streng i Go. En enkel metode er å bruke innebygde funksjoner som len() og utf8.RuneCountInString(). Begge disse funksjonene tar inn en streng som argument og returnerer antall tegn i strengen. Her er et eksempel på hvordan dette kan se ut i kode:

```Go
// Lager en streng
streng := "Hei, verden!"

// Finner lengden på strengen ved hjelp av len() funksjonen
lengde := len(streng)

// Skriver ut resultatet
fmt.Printf("Lengden av strengen er %d tegn", lengde) // Output: Lengden av strengen er 12 tegn

// Finner lengden på strengen ved hjelp av utf8.RuneCountInString() funksjonen
lengde2 := utf8.RuneCountInString(streng)

// Skriver ut resultatet
fmt.Printf("Lengden av strengen er %d tegn", lengde2) // Output: Lengden av strengen er 12 tegn
```

## Dybdeanalyse

Når man bruker len() funksjonen, teller den hvert tegn i strengen som én tegn. Dette betyr at også spesialtegn og mellomrom blir talt med. Utf8.RuneCountInString() funksjonen derimot, teller antall Unicode-tegn i strengen, noe som kan gi et annerledes resultat enn len() funksjonen hvis det er spesialtegn eller ikke-engelske bokstaver i strengen.

Det finnes også en annen metode for å finne lengden på en streng, ved å bruke range-løkken. Denne løkken itererer gjennom hver rune i strengen og teller dem. Dette kan være en nyttig løsning hvis du trenger å behandle hver rune individuelt, men kan være mindre effektivt enn de innebygde funksjonene.

## Se også

- [Offisiell Go-dokumentasjon for strenger](https://golang.org/pkg/strings/)
- [Tutorial om styring av strenger i Go](https://gobyexample.com/strings)
- [Eksempel på hvordan range-løkken kan brukes til å finne strenglengde](https://golangbyexample.com/finding-length-of-string-golang/)