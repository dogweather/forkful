---
title:    "Fish Shell: Lesing av tekstfil"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med Fish Shell, er det viktig å kunne lese og håndtere tekstfiler. Dette kan være nyttig for å behandle data eller få tilgang til konfigurasjonsfiler. Les videre for å lære hvordan du kan lese tekstfiler i Fish Shell.

## Slik gjør du det

Det er flere måter å lese en tekstfil på i Fish Shell, men den enkleste måten er å bruke `cat` kommandoen. Denne kommandoen vil skrive innholdet i filen rett til terminalen. For eksempel hvis du vil lese innholdet i en fil som heter "tekstfil.txt", kan du bruke følgende kommando:

```Fish Shell
cat tekstfil.txt
```

Dette vil skrive ut innholdet i filen til terminalen. Hvis du vil lagre innholdet i en variabel, kan du bruke "set" kommandoen i følgende format:

```Fish Shell
set min_variabel (cat tekstfil.txt)
```

Dette vil lagre innholdet i filen i variabelen "min_variabel".

Hvis du vil lese innholdet i filen linje for linje, kan du bruke `head` og `tail` kommandoene. For eksempel, hvis du vil lese de første fem linjene i en fil, kan du bruke følgende kommando:

```Fish Shell
head -n 5 tekstfil.txt
```

På samme måte kan du bruke `tail` kommandoen for å lese de siste fem linjene i en fil.

## Dypere dykk

Hvis du ønsker å utføre flere operasjoner på innholdet i en tekstfil, er det også mulig å bruke `grep` kommandoen. Denne kommandoen lar deg søke gjennom filen etter et bestemt mønster. For eksempel, hvis du vil finne alle linjer som inneholder ordet "Fish", kan du bruke følgende kommando:

```Fish Shell
grep "Fish" tekstfil.txt
```

Dette vil skrive ut alle linjene som matcher det gitte mønsteret.

## Se også

- [Fish Shell søkeeksempler](https://fishshell.com/docs/current/tutorial.html#guide-example1)
- [Fish Shell variabler](https://fishshell.com/docs/current/tutorial.html#variables)