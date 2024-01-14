---
title:                "Fish Shell: Skriver til standardfeil"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Innen programmering er det viktig å kunne håndtere feil, og dette gjelder spesielt når det kommer til å programmere med Fish Shell. En måte å håndtere feil på er å skrive til standard error (stderr) i stedet for standard output (stdout). Dette kan være nyttig for å fange og behandle eventuelle feilmeldinger som kan oppstå under kjøring av et program.

## Slik gjør du det

For å skrive til standard error i Fish Shell, kan du bruke kommandoen `echo` etterfulgt av teksten du ønsker å skrive til stderr, og så rute det til stderr ved hjelp av symbolet `>`.

```Fish Shell
echo "Dette er en feilmelding" > stderr
```

Dette vil skrive teksten "Dette er en feilmelding" til standard error. Du kan også bruke `>`-symbolet sammen med andre kommandoer som kan generere feil, for eksempel `ls`:

```Fish Shell
ls filsomikkeeksisterer > stderr
```

Dette vil skrive feilmeldingen "ls: cannot access 'filsomikkeeksisterer': No such file or directory" til standard error.

## Dykk dypere

Skriving til standard error kan også være nyttig for å skrive ut feil fra et skript eller program. For å få tak i feilmeldinger fra et skript, kan du bruke kommandoen `2>&1` for å rute både stderr og stdout til samme sted.

```Fish Shell
./skript.sh 2>&1
```

Dette lar deg fange og håndtere eventuelle feilmeldinger som skriptet kan generere.

## Se også

Hvis du ønsker å lære mer om hvordan du håndterer feil i Fish Shell, kan disse ressursene være nyttige:

- [Fish Shell dokumentasjon om stderr](https://fishshell.com/docs/current/tutorial.html#err)
- [Fish Shell Stack Overflow forum](https://stackoverflow.com/questions/tagged/fish)
- [Fish Shell Slack-samfunn](https://fishshell.com/docs/current/tutorial.html#slack)