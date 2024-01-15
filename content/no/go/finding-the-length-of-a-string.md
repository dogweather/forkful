---
title:                "Å finne lengden på en streng."
html_title:           "Go: Å finne lengden på en streng."
simple_title:         "Å finne lengden på en streng."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen være interessert i å finne lengden på en streng? Vel, det kan være mange grunner til det. Kanskje du vil validere inndata fra brukere, eller kanskje du trenger å begrense tekstlengden på et feltskema. Å vite hvordan du finner lengden på en streng kan være en nyttig ferdighet å ha i verktøykassen din som en Go-utvikler.

## Hvordan

For å finne lengden på en streng i Go, kan du bruke len() funksjonen. Denne funksjonen tar inn en streng som parameter og returnerer et heltall som representerer antall tegn i strengen. La oss se på et eksempel:

```Go
s := "Hei alle sammen!"
lengde := len(s)
fmt.Println(lengde)
```

Dette vil gi følgende resultat:

```
16
```

Her ser du at s strengen har en lengde på 16 tegn. Det kan også være lurt å håndtere spesielle tegn eller unicode i strenger når du regner ut lengden. Du kan bruke utf8.RuneCountInString() funksjonen for å unngå problemer med spesielle tegn.

## Dypdykk

Når du bruker len() funksjonen, må du være oppmerksom på at den returnerer antall bytes og ikke antall tegn i strengen. Dette kan føre til noen utilsiktede resultater hvis du håndterer unicode eller spesielle tegn i strenger. For eksempel, hvis du har en streng med både engelske og kinesiske tegn, kan antall bytes være større enn antall tegn.

Du kan også bruke range løkken for å finne lengden på en streng. Dette vil returnere antall tegn i strengen, i stedet for antall bytes. Her er et eksempel på å bruke range løkken for å finne lengden på en streng:

```Go
s := "你好世界!"
var lengde int
for _, c := range s {
  lengde++
}
fmt.Println(lengde) // ville gi "5" som resultat, siden det er 5 kinesiske tegn i strengen
```

## Se også

For flere tips og triks når det kommer til å jobbe med strenger i Go, kan du se på disse ressursene:

- [Official Go language documentation on strings](https://golang.org/pkg/strings/)
- [A Tour of Go - Strings](https://tour.golang.org/basics/5)
- [String functions in Go](https://www.callicoder.com/golang-strings-guide/)