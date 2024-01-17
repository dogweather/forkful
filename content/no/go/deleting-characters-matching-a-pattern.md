---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "Go: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sletting av tegn som matcher et mønster er en vanlig metode blant programmerere for å manipulere tekststrenger. Dette gjøres for å skape en mer ønsket utdata eller for å fjerne unødvendige tegn.

## Hvordan:
Koding eksempler og eksempelutgang innenfor ```Go...``` kodeblokker:
```
// Slett alle tall fra en tekststreng
tekst := "H3llo W0rld"
nyTekst := regexp.MustCompile("[0-9]").ReplaceAllString(tekst, "")
fmt.Println(nyTekst)
// Resultat: Hello World

// Slett alle tegn som ikke er bokstaver eller tall
tekst := "Hello! 123 World?"
nyTekst := regexp.MustCompile("[^a-zA-Z0-9]").ReplaceAllString(tekst, "")
fmt.Println(nyTekst)
// Resultat: Hello123World
```

## Dypdykk:
Et mønster for å matche og slette tegn ble først brukt i Unix verktøyet "ed" på 1970-tallet. Siden da har det blitt en vanlig funksjon i de fleste programmeringsspråk, inkludert Go. Alternativene til å slette tegn i Go inkluderer også "Trim" og "TrimFunc" funksjonene, som fjerner spesifiserte tegn eller funksjoner fra begynnelsen og slutten av en tekststreng. Implementasjonsdetaljer for å slette tegn i Go kan bli funnet i standard "regexp" pakken.

## Se også:
[Go regexp pakken](https://golang.org/pkg/regexp/)