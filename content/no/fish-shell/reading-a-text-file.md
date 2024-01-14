---
title:                "Fish Shell: Lese en tekstfil"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er interessert i å forstå hvordan Fish Shell fungerer og ønsker å lære mer om hvordan du kan lese og manipulere tekstfiler, så er denne bloggposten for deg.

## Hvordan

Du kan lese en tekstfil ved å bruke `cat`-kommandoen i Fish Shell, som står for "concatenate". Det betyr at filen vil bli skrevet ut på skjermen. For eksempel, hvis du vil lese filen "tekst.txt", skriv følgende i Fish Shell:

```Fish Shell
cat tekst.txt
```

Dette vil gi deg hele innholdet i tekstfilen på skjermen.

Du kan også bruke `head`-kommandoen for å bare vise de første linjene i en tekstfil. For eksempel:

```Fish Shell
head -5 tekst.txt
```

Dette vil vise de første fem linjene i tekstfilen.

For å søke etter et bestemt ord eller uttrykk i en tekstfil, kan du bruke `grep`-kommandoen. For eksempel, hvis du vil søke etter ordet "Fish" i tekstfilen, skriv følgende:

```Fish Shell
grep Fish tekst.txt
```

Dette vil gi deg alle linjene som inneholder ordet "Fish".

## Deep Dive

For å utforske mer avanserte måter å lese tekstfiler på, kan du også bruke forskjellige flagg og kombinere flere kommandoer i Fish Shell. For eksempel kan du bruke `awk`-kommandoen for å filtrere ut spesifikke deler av en tekstfil basert på et gitt mønster. Du kan også bruke `sed`-kommandoen for å manipulere og endre innholdet i en tekstfil.

Det er også viktig å merke seg at Fish Shell har innebygde variabler som gjør det enklere å arbeide med tekstfiler. Du kan bruke `$argv` for å lese argumenter gitt til en kommando, og `$status` for å få tilbakemelding om statusen til den siste kommandoen som ble utført.

## Se også

- [Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix Text Processing Commands](https://www.tutorialspoint.com/unix/unix-regular-expressions.htm)
- [Bash vs Fish: Which is the Best Shell?](https://www.freecodecamp.org/news/bash-vs-fish-shell-which-is-the-best-shell-fd2cc0ba8b07/)