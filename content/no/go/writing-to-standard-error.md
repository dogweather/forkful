---
title:                "Skriver til standardfeil"
html_title:           "Go: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive til standard feil er en måte for utviklere å fange og håndtere feil i sitt Go-programmeringsspråk. Dette lar programmerere få en detaljert rapport om feil og hjelper med å feilsøke og forbedre koden sin.

## Hvordan:

For å skrive til standard feil i Go, bruker du fmt.Fprintf() -funksjonen og angir standard feil som det første argumentet, etterfulgt av en formatstreng og eventuelle verdier du vil skrive ut. Se eksempelet nedenfor:

```
fmt.Fprintf(os.Stderr, "Feil oppstod: %s", err)
```

Dette vil skrive ut feilmeldingen til standard feil, som vanligvis er konsollen når du kjører programmet fra terminalen. Her er et annet eksempel som skriver ut til standard feil på en enklere måte:

```
fmt.Fprintln(os.Stderr, "Oisann, en feil skjedde!")
```

Begge eksemplene ovenfor vil skrive ut teksten til standard feil og legge til en linjeskift etterpå.

## Dypdykk:

Skrive til standard feil har eksistert siden de første dager av programmering og er en vanlig måte å håndtere feil på. Alternativer til dette er for eksempel å logge feil i en fil eller sende en e-post med feilmeldingen. Selv om dette kan være mer pålitelig, er det også mer komplekst og krever ekstra kode og ressurser.

Go sitt innebygde fmt-pakke gjør det enkelt å skrive til standard feil, og det er derfor valgt av mange utviklere som en effektiv og enkel måte å håndtere feil på.

## Se også:

- [Official Go documentation for fmt package](https://golang.org/pkg/fmt/)
- [Error handling in Go: Best Practices](https://blog.golang.org/error-handling-and-go)