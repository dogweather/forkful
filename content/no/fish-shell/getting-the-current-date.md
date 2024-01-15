---
title:                "Hente nåværende dato"
html_title:           "Fish Shell: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Så hvorfor skulle noen bry seg om å få den nåværende datoen? Tja, det kan være flere grunner til det. Kanskje du ønsker å sette opp en automatisk påminnelse for et viktig møte eller en begivenhet, eller kanskje du bare vil følge med på hvilken dag det er. Uansett grunn, så er det alltid nyttig å kunne få den nåværende datoen raskt og enkelt.

## Slik gjør du det

For å få den nåværende datoen i Fish Shell, kan du bruke innebygde funksjoner som kalles variabler. Variabelen "today" vil gi deg den nåværende datoen i formatet "dd.mm.yyyy". Du kan bruke denne variabelen i dine kommandoer, for eksempel:

```Fish Shell
echo I dag er $today
```

Dette vil skrive ut "I dag er [dato]" i terminalen din. Du kan også tilpasse måten datoen blir vist på ved å bruke "date" kommandoen. For eksempel, hvis du vil ha datoen i formatet "mm/dd/yy", kan du bruke:

```Fish Shell
echo Dagens dato er (date +%m/%d/%y)
```

Dette vil skrive ut "Dagens dato er [dato]". Som du kan se, vil "date" kommandoen fungere som en funksjon som returnerer datoen i ønsket format.

Et annet nyttig triks er å få den nåværende dagen i uken. Dette kan gjøres ved å bruke variabelen "day". For eksempel:

```Fish Shell
echo I dag er det $day
```

Dette vil skrive ut "I dag er det [dag i uken]". Du kan kombinere denne variabelen med "date" kommandoen for å få en fullstendig uttrykt dato og dag i uken.

## Grav dypere

Hvis du vil ha mer detaljert informasjon om datoen, kan du bruke "cal" kommandoen. Denne kommandoen vil vise kalenderen for den nåværende måneden, og du kan også velge å vise kalenderen for en spesifikk måned og år hvis ønskelig. Ved å bruke "cal" kommandoen med "--year" og "--month" flagg kan du spesifisere året og måneden for kalenderen du vil se.

En annen nyttig kommando er "date -I". Dette vil gi deg datoen i det internasjonale formatet "yyyy-mm-dd". Dette er nyttig hvis du trenger å sortere eller sammenligne datoer i et skript.

## Se også

- [Fish Shell documentation](https://fishshell.com/docs/current/)
- [A Beginner's Guide to Fish Shell](https://www.freecodecamp.org/news/a-beginners-guide-to-fish-shell/)