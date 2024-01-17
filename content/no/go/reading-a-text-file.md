---
title:                "Leser en tekstdokument"
html_title:           "Go: Leser en tekstdokument"
simple_title:         "Leser en tekstdokument"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Lesing av tekstfiler er en vanlig oppgave for programmerere, og det er en enkel måte å lese og behandle store mengder tekstbasert data. Dette kan være nyttig for å hente informasjon fra loggfiler, eksportere data til en database eller lage rapporter.

## Hvordan:

Go har en innebygd pakke kalt "os" som gjør det enkelt å lese en tekstfil. Vi kan bruke funksjonen "Open" sammen med "Read" og "Close" for å åpne, lese og lukke filen vår. Her er et enkelt eksempel på hvordan du kan lese en tekstfil og skrive ut innholdet:

```Go
f, err := os.Open("filnavn.txt")
if err != nil {
    panic(err)
}
defer f.Close()

b := make([]byte, 50)
n, err := f.Read(b)
if err != nil {
    panic(err)
}
fmt.Println(string(b))
```

Output: "Dette er en tekstfil"

## Dypdykk:

Å lese tekstfiler er en vanlig oppgave, og det finnes flere måter å gjøre det på i Go. Du kan også bruke funksjonen "ioutil.ReadFile" eller bruke en scanner fra pakken "bufio". Det er også viktig å merke seg at du må utføre en feilhåndtering når du leser filen, slik at programmet ikke krasjer hvis filen ikke kan leses.

## Se også:

For en mer detaljert forklaring og flere eksempler, sjekk ut denne artikkelen på Go sin nettside: https://golang.org/pkg/os/#File.Read

Du kan også lese mer om forskjellige måter å lese tekstfiler på i denne artikkelen: https://golang.org/doc/tutorial/introduction#Files