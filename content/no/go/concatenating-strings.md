---
title:    "Go: Sammenslåing av tekststrenger"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Concatenate, eller slå sammen, av tekststrenger er en vanlig operasjon i programmering, spesielt i Go. Ved å kombinere forskjellige tekststrenger kan du lage komplekse og innholdsrike meldinger og utskrifter. 

## Hvordan

Kodingen for å slå sammen tekststrenger i Go er enkel og intuitiv. Ved hjelp av operatoren `+` kan du enkelt slå sammen to eller flere tekststrenger. For eksempel:

```Go
navn := "Nora"
tilhilsen := "Hei, jeg heter " + navn + " og jeg er fra Norge."
fmt.Println(tilhilsen)
```
```
Hei, jeg heter Nora og jeg er fra Norge.
```

Du kan også bruke `+=` operator for å legge til tekststrenger til en eksisterende variabel. For eksempel:

```Go
greeting := "Hei, "
name := "Nora"
greeting += name
fmt.Println(greeting)
```
```
Hei, Nora
```

Det er viktig å merke seg at når du slår sammen tekststrenger, må de være av samme type. Hvis ikke vil du få en kompileringsfeil.

## Dypdykk

Når du slår sammen tekststrenger i Go, blir det laget en ny strengverdi hver gang. Dette kan påvirke ytelsen hvis du har en stor mengde tekster som skal slås sammen. Derfor anbefales det å bruke `strings.Join()` funksjonen for å slå sammen store mengder tekststrenger.

```Go
tekststrenger := []string{"Hei", "på", "deg,", "håper", "du", "har", "en", "fin", "dag."}
slåttSammen := strings.Join(tekststrenger, " ")
fmt.Println(slåttSammen)
```
```
Hei på deg, håper du har en fin dag.
```

## Se også

- [Go Dokumentasjon om tekststrenger](https://golang.org/pkg/strings/)
- [Effektiv Go: Konkatinering av tekststrenger](https://golang.org/doc/effective_go.html#string_concat)