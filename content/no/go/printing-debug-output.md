---
title:                "Go: Utskrift av feilsøkingsdata"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive kode kan noen ganger føles som å løse en gåte. Problemet er at du ikke alltid vet svaret på gåten, og da kan det være nyttig å kunne se hva som skjer underveis i koden. Det er her utskrift av feilsøkingsdata kommer inn. Ved å skrive ut data til konsollen mens koden kjører, kan du få en bedre forståelse av hvordan koden din fungerer og mulige feil som oppstår. Det kan også være nyttig for å finne ut hvor koden din bremser eller hvorfor den ikke gjør det du forventer.

## Hvordan

I Go er det enkelt å skrive ut feilsøkingsdata ved hjelp av "fmt" pakken. For å komme i gang, må du importere "fmt" pakken øverst i filen din: 

```Go
import "fmt"
```

Så kan du bruke "fmt.Println()" funksjonen for å skrive ut data til konsollen. La oss si du har en variabel "navn" og vil skrive ut verdien av den. Du kan gjøre det slik:

```Go
navn := "Ole"
fmt.Println(navn)
```

Dette vil skrive ut "Ole" til konsollen. Du kan også kombinere tekst og variabler ved å bruke "fmt.Printf()" funksjonen. For eksempel, hvis du vil skrive ut "Hei, Ole!" vil du gjøre det slik:

```Go
navn := "Ole"
fmt.Printf("Hei, %s!", navn)
```

Dette vil skrive ut "Hei, Ole!" til konsollen. Du kan også legge til flere variabler ved å inkludere flere "%s" og passende variabelverdier i rekkefølgen de skal vises.

## Dypdykk

I tillegg til å skrive ut enkle variabler, kan du også skrive ut mer komplekse datastrukturer som arrays, maps og structs. For arrays og maps kan du bruke "fmt.Println()" eller "fmt.Printf()" funksjoner på samme måte som for enkeltvariabler. For structs kan du bruke "fmt.Printf()" funksjonen og referere til de ulike feltene i structet ved å bruke ".". For eksempel, hvis du har et struct som heter "Person" med feltene "navn" og "alder", vil du gjøre det slik:

```Go
person := Person{"Ole", 25}
fmt.Printf("Navn: %s, Alder: %d", person.navn, person.alder)
```

Dette vil skrive ut "Navn: Ole, Alder: 25" til konsollen.

Du kan også bruke "fmt.Sprintf()" funksjonen til å formatere en string og lagre den i en variabel, i stedet for å skrive den direkte til konsollen. Dette kan være nyttig hvis du vil inkludere feilsøkingsdata i en feilmelding eller skrive den til en fil.

## Se også

For flere detaljer om hvordan du bruker "fmt" pakken og utskrift av feilsøkingsdata i Go, kan du sjekke ut disse ressursene:

- https://golang.org/pkg/fmt/
- https://www.golang-book.com/books/intro/4