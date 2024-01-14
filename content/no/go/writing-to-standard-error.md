---
title:                "Go: Skriving til standardfeil"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en viktig og vanlig måte å håndtere feil og gi advarsler i Go-programmering. Det lar deg raskt og enkelt kommunisere informasjon til brukeren om uventede situasjoner i koden din.

## Slik gjør du det

For å skrive til standard error i Go, kan du bruke `fmt.Fprintf()` funksjonen og spesifisere `os.Stderr` som det første argumentet. Du kan deretter legge til din egen melding eller variabel etterfulgt av en linjeskift:

```Go
fmt.Fprintf(os.Stderr, "Feil: %v\n", err)
```

Dette vil skrive ut en melding på standard error i stedet for den vanlige standard output. Du kan også bruke `fmt.Fprintln()` funksjonen for å legge til en linjeskift på slutten av meldingen.

## Dypdykk

I Go-programmering er standard error en av de tre standardstrømmene som brukes til å kommunisere med omverdenen. De tre er standard input, standard output og standard error. Standard error brukes spesielt til å rapportere feil og advarsler, mens standard output brukes til å skrive ut vanlig output.

Hvis du ønsker å lese fra standard error, kan du bruke `os.Stderr` som en `io.Reader` og bruke `bufio` pakken for å lese linje for linje:

```Go
reader := bufio.NewReader(os.Stderr)
line, err := reader.ReadString('\n')
```

For å skrive til standard out istedenfor standard error, kan du bruke `os.Stdout` i `fmt.Fprintf()` funksjonen.

## Se også

- [Offisiell Go-dokumentasjon om fprintf](https://golang.org/pkg/fmt/#Fprintf)
- [Go Unofficial Code Style Guide - Writing to Standard Error](https://github.com/golang/go/wiki/CodeReviewComments#write-to-standard-error)