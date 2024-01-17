---
title:                "Konvertere en dato til en streng"
html_title:           "Go: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av dato til en streng betyr å endre datoen til tekstformat, for eksempel fra "22/10/2021" til "22. oktober 2021". Dette er nyttig for å presentere datoer på en mer lesbar måte for brukere. Programmere gjør dette for å øke brukervennligheten og for å unngå feil ved presentasjon av datoer.

## Hvordan:
For å konvertere en dato til streng i Go, bruker vi funksjonen `format()` fra `time` pakken. Vi gir funksjonen en dato og en ønsket strengformat som argumenter, og den vil returnere datoen i strengformatet. Her er et eksempel:

```Go
import "time"

func main() {
    date := time.Date(2021, 10, 22, 0, 0, 0, 0, time.UTC)
    strDate := date.Format("2. January 2006")
    fmt.Println(strDate)
}
```
Output: `22. October 2021`

## Dypdykk:
Konvertering av dato til streng er ikke en ny konsept, og har vært en vanlig oppgave i programmering i lang tid. Alternativt kan også `strconv` pakken brukes til å konvertere datoer til strenger i Go. Det finnes også ulike formateringsmuligheter for datoer i Go, som kan utforskes nærmere i Go-dokumentasjonen.

## Se også:
- https://gobyexample.com/string-formatting
- https://pkg.go.dev/time#pkg-overview