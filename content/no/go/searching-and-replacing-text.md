---
title:    "Go: Søke og erstatte tekst"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst i programmering kan være en viktig del av å lage effektive og rene kodeløsninger. Det er en rask og enkel måte å gjøre endringer i store tekstfiler eller kodeprosjekter.

## Hvordan

For å søke og erstatte tekst i Go, bruker vi funksjonen `strings.Replace`. Denne funksjonen tar inn en inputstreng, søkeord, erstatningsord og antall forekomster som skal erstattes. La oss se på et eksempel:

```Go
input := "Velkommen til Go Programmering"
søkeord := "Go"
erstatningsord := "Golang"

output := strings.Replace(input, søkeord, erstatningsord, 1)

fmt.Println(output)
```
Output:
```
Velkommen til Golang Programmering
```

I dette eksempelet vil søkeordet "Go" bli erstattet med "Golang" én gang i inputstrengen.

## Dypdykk

I tillegg til å erstatte tekst, kan vi også søke etter tekst ved hjelp av funksjonen `strings.Contains`. Denne funksjonen returnerer en boolsk verdi basert på om søkeordet finnes i inputstrengen eller ikke.

Vi kan også bruke `strings.ContainsAny` for å søke etter flere ulike ord eller tegn. Denne funksjonen tar inn en liste over søkeord og returnerer en boolsk verdi basert på om noen av søkeordene finnes i inputstrengen.

En annen nyttig funksjon for å håndtere tekst i Go er `strings.Split`, som deler en streng basert på et gitt tegn eller ord og returnerer en liste over deler.

Det er også verdt å nevne funksjonen `strings.Join` som kombinerer en liste av strenger til én streng, ved hjelp av et gitt tegn som separator.

## Se også

- [Offisiell Go strings-dokumentasjon](https://golang.org/pkg/strings/)
- [Go by Examples - Strings](https://gobyexample.com/strings)
- [Learn Go in Y minutes - Strings](https://learnxinyminutes.com/docs/go/#strings)