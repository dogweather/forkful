---
title:                "Generering av tilfeldige tall"
html_title:           "Go: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å generere tilfeldige tall? Det kan være nyttig for mange typer programmering, for eksempel simulasjon, tilfeldig utvelgelse av data eller for å legge til en tilfeldig faktor i et spill.

## Hvordan

Det finnes flere måter å generere tilfeldige tall i Go. Her er to eksempler:

```Go
// Package "math/rand" må importeres for å bruke tilfeldige tall.
import "math/rand"
// For å få ulike tall hver gang programmet kjøres, kan man bruke funksjonen "Seed".
rand.Seed(time.Now().UnixNano())
// Deretter kan man bruke funksjonen "Intn" for å generere et heltall mellom 0 og et gitt tall.
rand.Intn(100) // Dette vil returnere et tilfeldig tall mellom 0 og 99.
```

En annen måte å generere tilfeldige tall på er ved å bruke pakken "crypto/rand". Denne pakken genererer cryptografisk sikre tilfeldige tall, noe som kan være viktig for sikkerhetsfølsomme programmer.

```Go
// Package "crypto/rand" må importeres for å bruke tilfeldige tall.
import "crypto/rand"
// For å generere et heltall mellom 0 og et gitt tall, kan man bruke funksjonen "Int".
// Denne funksjonen tar to argumenter: en Reader og et heltall som angir øvre grense for det tilfeldige tallet.
rand.Int(rand.Reader, 100) // Dette vil returnere et tilfeldig heltall mellom 0 og 99.
```

## Dypdykk

Mens de to eksemplene ovenfor gir en enkel måte å generere tilfeldige tall på, er det viktig å forstå at disse tallene ikke er 100% tilfeldige. Tilfeldige tall som genereres av datamaskiner er faktisk pseudotilfeldige, noe som betyr at de følger en bestemt algoritme og kan forutsies. For å få tettere på ekte tilfeldighet, må man bruke eksterne faktorer som støkastiske prosesser eller fysiske fenomener.

For å lære mer om hvordan tilfeldige tall genereres i Go, kan du sjekke ut følgende ressurser:
- [Offisiell dokumentasjon for tilfeldige tall i Go](https://golang.org/pkg/math/rand/)
- [Artikkel om generering av tilfeldige tall i Go](https://medium.com/@sagarsawant/generating-random-numbers-most-proper-way-in-go-lang-9fbe6f5f2c3a)
- [Stack Overflow-tråd om tilfeldige tall i Go](https://stackoverflow.com/questions/12321133/generating-random-numbers-in-go)
- [Video-foredrag om tilfeldige tall i Go](https://www.youtube.com/watch?v=ezk6VYUl0F4)

## Se også

- [Offisiell dokumentasjon for Go](https://golang.org/doc/)
- [Go-tutorial for nybegynnere](https://tour.golang.org/welcome/1)
- [Lær Go gjennom interaktive oppgaver](https://gophercises.com/)