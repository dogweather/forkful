---
title:    "Bash: Utskrift av feilsøkingsutdata"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Utskrift av feilsøkingsmeldinger er en viktig del av Bash programmering. Det lar deg se hva som skjer under kjøring av koden din og finne feil eller ineffektiviteter som kan forbedres. Det er også en nyttig måte å forstå nøyaktig hva som skjer i koden din og hvordan den reagerer på forskjellige inputs.

## Hvordan

For å skrive ut en feilsøkingsmelding i Bash, kan du bruke kommandoen "echo". En enkel måte å gjøre dette på er å inkludere variabelen "$PS4" før du kjører kommandoen "set -x" for å aktivere sporing av kommandoene. Dette vil skrive ut en linje med koden som kjøres før hver kommando som gir deg mer kontroll over utskriftsprosessen.

```Bash
$ set -x
$ PS4='$LINENO: '
$ echo "Dette er en feilsøkingsmelding"
```

Dette vil gi følgende output:

```Bash
1: echo "Dette er en feilsøkingsmelding"
Dette er en feilsøkingsmelding
```

Et annet nyttig verktøy for å skrive ut feilsøkingsmeldinger er "printf" kommandoen. Dette lar deg formatere utskriften slik du vil og inkludere variabler og andre data i meldingen. For eksempel:

```Bash
printf "Variabelen heter %s og verdien er %d" $variabel $verdi
```

Dette vil skrive ut følgende:

```Bash
Variabelen heter variabel og verdien er 10
```

## Dypdykk

For å få enda mer kontroll over utskriftsprosessen kan du bruke "enable -x" kommandoen som lar deg angi en funksjon som skal kjøres før hver kommando. Dette kan være nyttig hvis du vil ha en mer tilpasset feilsøkingsmelding eller må ta hensyn til forskjellige inputs.

En annen nyttig teknikk er å bruke ">&2" for å skrive til standard error i stedet for standard output. Dette kan være nyttig hvis du vil skille utskrifter for feilsøking fra f.eks. vanlige feilmeldinger som skrives ut til standard error.

Disse teknikkene kan være nyttige for å finne problematiske områder i koden din og forbedre effektivitet og feilhåndtering.

## Se også

- [Echo Command in Linux with Examples](https://www.tecmint.com/echo-command-in-linux/)
- [The printf Command in Linux](https://www.linux.com/news/printf-command-linux/) 
- [Debugging Bash Scripts – Part 1: Commands & Functions](https://www.linuxjournal.com/content/debugging-bash-scripts-part-1-commands-functions)