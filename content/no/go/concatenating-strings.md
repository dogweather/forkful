---
title:                "Go: Kombinering av strenger"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

I mange programmeringsspråk kan man bruke en operatør, som for eksempel "+" for å kombinere to strenger. I Go, må man bruke funksjonen "Concat" for å gjøre det samme. Hvorfor skulle man velge å bruke denne funksjonen i stedet for en enkel operatør? Fordi det å bruke "Concat" kan ha noen fordeler som vi skal se nærmere på under.

## Hvordan

Golang tilbyr flere måter å kombinere strenger på, men vi skal fokusere på én spesifik metode: ved hjelp av "Concat" funksjonen. Det første vi må gjøre er å importere "strings" pakken:

```
import "strings"
```

Så kan vi bruke "Concat" funksjonen som følger:

```
resultat := strings.Concat("Hei", "verden!")
fmt.Println(resultat)
```

Output:

```
Hei verden!
```

Som du kan se, tar "Concat" to strenger som parametere og returnerer en ny streng som er en kombinasjon av de to.

Vi kan også bruke "Concat" til å kombinere flere strenger på en enkelt linje:

```
resultat := strings.Concat("Hvordan går ", "det ", "i ", "dag?")
fmt.Println(resultat)
```

Output:

```
Hvordan går det i dag?
```

Det er også verdt å merke seg at "Concat" funksjonen også kan ta imot variadic argumenter, noe som betyr at vi kan gi den et ubegrenset antall strenger å kombinere, for eksempel:

```
resultat := strings.Concat("Velkommen", "til", "Go", "programmering", "!")
fmt.Println(resultat)
```

Output:

```
Velkommen til Go programmering!
```

## Dypdykk

I tillegg til å kombinere strenger, kan "Concat" funksjonen også brukes til å legge til et mellomrom mellom hver streng. For å gjøre dette, kan vi bruke "Join" funksjonen som er en del av "strings" pakken.

La oss bruke følgende eksempel:

```
strenger := []string{"Dette", "er", "en", "setning."}
resultat := strings.Join(strenger, " ")
fmt.Println(resultat)
```

Output:

```
Dette er en setning.
```

Vi kan også bruke "Concat" og "Join" funksjonene sammen for å legge til en prefiks eller suffiks til en streng. For eksempel:

```
streng := "Golang"
streng = strings.Concat("Velkommen til ", streng)
streng = strings.Join([]string{streng, "!"}, "")
fmt.Println(streng)
```

Output:

```
Velkommen til Golang!
```

Som du kan se, kan "Concat" og "Join" funksjonene være nyttige verktøy i Go-programmering når det kommer til å kombinere og manipulere strenger.

## Se også

- https://golang.org/pkg/strings/#Concat
- https://golang.org/pkg/strings/#Join
- https://gobyexample.com/string-concatenation