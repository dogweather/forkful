---
title:    "Fish Shell: Utskrift av feilsøkingsresultater"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut feilsøkingsutdata kan være en viktig del av utviklingsprosessen for å finne feil og forbedre koden din. Det kan hjelpe deg med å identifisere hvor programmet ditt mislykkes og hvorfor det gjør det.

## Hvordan gjøre det

For å skrive ut feilsøkingsutdata i Fish Shell, kan du bruke kommandoen `echo` etterfulgt av teksten du ønsker å skrive ut. For eksempel:

```Fish Shell
echo "Dette er et eksempel på feilsøkingsutdata."
```

Dette vil skrive ut teksten "Dette er et eksempel på feilsøkingsutdata." i terminalen når du kjører programmet ditt.

En annen måte å skrive ut feilsøkingsutdata på er å bruke kommandoen `printf` etterfulgt av teksten du ønsker å skrive ut. For eksempel:

```Fish Shell
printf "Verdi 1: %d\nVerdi 2: %d" $var1 $var2
```

Dette vil skrive ut verdiene av variablene `var1` og `var2` sammen med teksten "Verdi 1:" og "Verdi 2:".

Det er også mulig å skrive ut feilsøkingsutdata til en fil ved å bruke `echo` eller `printf` og omdirigere utdata til en fil ved hjelp av `>` operatøren. For eksempel:

```Fish Shell
echo "Dette er et eksempel på feilsøkingsutdata." > feilsøkingsutdata.txt

printf "Verdi 1: %d\nVerdi 2: %d" $var1 $var2 > feilsøkingsutdata.txt
```

Dette vil skrive ut teksten eller verdiene til en fil kalt "feilsøkingsutdata.txt".

## Dypdykk

Det er flere formateringsalternativer du kan bruke når du skriver ut feilsøkingsutdata. For eksempel kan du bruke `echo -e` kommandoen for å vise spesielle tegn som linjeskift eller tabuleringer. Du kan også bruke variabler i utdataen din ved hjelp av `$` tegnet.

I tillegg kan du bruke betingede uttrykk i `echo` eller `printf` for å kun skrive ut utdata hvis en gitt betingelse er oppfylt. Dette kan være nyttig for å skrive ut informasjon bare når noe spesielt skjer i programmet ditt.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/)
- [Guide til feilsøkingsutdata i Fish Shell](https://dev.to/thenemoscope/debugging-your-code-using-fish-shell-41i)
- [Eksempler på feilsøkingsutdata i Fish Shell](https://www.maketecheasier.com/printing-debug-output-in-fish-shell/)