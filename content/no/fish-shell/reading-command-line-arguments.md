---
title:                "Lesing av kommandolinje argumenter"
html_title:           "Fish Shell: Lesing av kommandolinje argumenter"
simple_title:         "Lesing av kommandolinje argumenter"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen ønske å lese kommandolinjeargumenter? Vel, det er faktisk en ganske nyttig funksjon i Fish Shell som kan hjelpe deg med å ta kommandoene dine til neste nivå. Enten du er en nybegynner i programmering verden, eller en erfaren veteran, kan det være nyttig å lære hvordan man leser kommandolinjeargumenter i Fish Shell.

# Slik gjør du det

Det er enkelt å lese kommandolinjeargumenter i Fish Shell ved å bruke en enkel kommando:

```Fish Shell
set arg (commandline -p arg)
```

La oss bryte det ned. Først bruker vi kommandoen "set" til å skape et variabelt som heter "arg". Deretter bruker vi "commandline -p" for å hente ut kommandolinjeargumentene og plasserer dem i "arg" variabelen. Kjører du dette i terminalen din, vil du se at "arg" nå inneholder alle de tilgjengelige kommandolinjeargumentene i en liste.

For å få en bedre forståelse av hvordan dette fungerer og hvordan du kan bruke det, la oss se på et eksempel. Si for eksempel at du har en scriptfil kalt "list_files.fish" som inneholder følgende:

```Fish Shell
for file in (ls)
    echo $file
end
```

Dette scriptet vil liste opp alle filene i nåværende mappe. Men, hva om du vil bruke det på en spesifikk mappe som du passerer som et kommandolinjeargument? Her er hvor lesing av kommandolinjeargumenter kommer til nytte:

```Fish Shell
set arg (commandline -p arg)
for file in (ls $arg)
    echo $file
end
```

Nå kan du kjøre scriptet ved å skrive følgende i terminalen:

```bash
./list_files.fish /sti/til/mappen
```

Dette vil liste opp alle filene i den spesifikke mappen du har passert som et kommandolinjeargument.

Det er verdt å merke seg at "commandline" kommandoen også har andre nyttige alternativer, som for eksempel "-c" for å få ut antall kommandolinjeargumenter og "-e" for å få ut den faktiske kommandolinjen som ble brukt.

# Dypdykk

Nå som du kjenner til hvordan du kan lese kommandolinjeargumenter i Fish Shell, kan vi gå litt dypere inn i emnet. Det er også verdt å nevne at du ikke bare kan lese kommandolinjeargumenter, men også skrive dem.

Ved hjelp av "-C" alternativet til "commandline" kommandoen, kan du endre kommandolinjen rett fra Fish Shell, noe som kan være veldig nyttig når du jobber med kraftige kommandoer eller oppretter skall-aliaser.

For mer informasjon om lesing og skriving av kommandolinjeargumenter i Fish Shell, kan du sjekke ut dokumentasjonen på deres offisielle nettside.

# Se også

- [Offisiell dokumentasjon for kommandolinjeargumenter i Fish Shell](https://fishshell.com/docs/current/cmds/commandline)
- [En guide til Fish Shell for nybegynnere](https://www.freecodecamp.org/news/fish-shell-beginner-tutorial/)
- [En mer avansert dypdykk i Fish Shell](https://www.mattjones.tech/blog/advanced-fish-fishshell/)