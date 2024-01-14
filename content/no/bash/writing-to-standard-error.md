---
title:                "Bash: Skriver til standardfeil"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle du skrive til standardfeil i Bash-programmering? Det kan være et nyttig verktøy for å kommunisere informasjon og feilmeldinger til brukere og utviklere. Ved å skrive til standardfeil kan du sørge for at viktig informasjon blir kommunisert, selv om det oppstår feil i programmet ditt.

## Hvordan

For å skrive til standardfeil i Bash-bruker vi kommandoen `echo` etterfulgt av teksten vi ønsker å skrive ut:

```bash
echo "Dette er en feilmelding" 1>&2
```

Her vil `1>&2` dirigere utdaten vår til standardfeil i stedet for standard output. Dette er viktig for å sikre at meldingen blir vist som en feilmelding og ikke som vanlig tekst.

La oss se et eksempel på hvordan dette fungerer i et Bash-skript:

```bash
#!/bin/bash
# Dette er et enkelt skript som sjekker om et filnavn er en gyldig fil
if [ -e "$1" ]; then
  echo "Filen eksisterer"
else
  echo "Filen eksisterer ikke" 1>&2 # Her bruker vi standardfeil for å kommunisere feilen
fi
```

La oss prøve å kjøre skriptet med et gyldig filnavn og et ugyldig filnavn:

```bash
$ ./sjekk_filen.sh fil.txt
Filen eksisterer

$ ./sjekk_filen.sh fil2.txt
Filen eksisterer ikke
```

Som du ser, vil meldingen for ugyldig fil bli vist som en feilmelding, noe som gjør det enklere å identifisere og håndtere feilen.

## Deep Dive

Det finnes en annen måte å skrive til standardfeil på, nemlig ved å bruke `printf`-kommandoen i stedet for `echo`. Forskjellen mellom de to er at `echo` automatisk legger til en ny linje på slutten av utdaten, mens `printf` ikke gjør det. Dette kan være nyttig hvis du ønsker å kontrollere formateringen av teksten din, spesielt hvis du skal skrive ut tall eller variabler.

En annen ting å være klar over er at du kan dirigere utdaten din til både standardfeil og standard output ved å bruke `1>&2`. Dette kan være nyttig hvis du ønsker å vise en feilmelding samtidig som du fortsetter å skrive til standard output.

## Se også

- [Offisiell dokumentasjon for `echo` og `printf` i GNU Bash](https://www.gnu.org/software/bash/manual/html_node/Echoing.html)
- [Eksempelskript for å illustrere bruk av standardfeil i Bash-programmering](https://github.com/bkuhlmann/bbk-examples/tree/master/bash/standard-error)