---
title:    "Fish Shell: Slette tegn som matcher et mønster"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Det kan være flere grunner til å ville slette tegn som matcher et visst mønster. En mulig grunn kan være at man ønsker å rydde opp i en tekstfil eller å fjerne uønskede tegn fra en variabel i et skript.

## Hvordan

For å slette tegn som matcher et mønster i Fish Shell, kan man bruke kommandoen `string replace` sammen med et regulært uttrykk. La oss si at vi har en tekstfil som inneholder ordet "Fish", men vi vil fjerne bokstaven "h" fra dette ordet. Vi kan bruke følgende kommando:

```Fish Shell
string replace -r 'h' '' < tekstfil.txt
```

Dette vil gi følgende output:

```
Fis
```

Vi kan også bruke denne kommandoen i et skript for å fjerne uønskede tegn fra en variabel. La oss si at vi har en variabel `$s` som inneholder teksten "12345678", men vi ønsker å fjerne alle tall som er større enn 5. Vi kan bruke følgende kommando:

```Fish Shell
s = (string replace -r '[6-9]' '' $s)
```

Når vi skriver ut variabelen `$s` vil vi få følgende output:

```
12345
```

Dette er bare noen enkle eksempler på hvordan man kan bruke `string replace` i Fish Shell for å slette tegn som matcher et mønster.

## Dypdykk

Det er verdt å merke seg at regulære uttrykk kan være ganske komplekse, og det kan være lurt å bruke et verktøy som [regex101](https://regex101.com/) for å teste og forstå uttrykkene man bruker. Det finnes også mange forskjellige flagg man kan bruke sammen med `string replace` for å få enda mer kontroll over søket, som for eksempel å ignorere store/små bokstaver og å kun søke på hele ord.

## Se også

- [Fish Shell documentation](https://fishshell.com/docs/current/commands.html#string-replace)
- [Regex101](https://regex101.com/)