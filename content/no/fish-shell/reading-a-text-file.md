---
title:                "Fish Shell: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan lese tekstfiler ved hjelp av Fish Shell? Da er du på rett sted! Dette innlegget vil vise deg hvordan du kan gjøre dette enkelt og effektivt.

## Hvordan

For å lese en tekstfil i Fish Shell, kan du bruke kommandoen `cat`. Denne kommandoen vil lese innholdet i filen og vise det i terminalvinduet. La oss si at vi har en tekstfil kalt "minfil.txt" som inneholder følgende tekst:

```
Hei! Velkommen til min blogg om Fish Shell.
```

For å lese denne filen i Fish Shell, skriver vi følgende kommando i terminalen:

```Fish Shell
cat minfil.txt
```

Dette vil gi oss følgende output:

```
Hei! Velkommen til min blogg om Fish Shell.
```

Du kan også bruke `less` kommandoen for å lese tekstfiler. Denne kommandoen vil gi deg muligheten til å bla gjennom tekstfilen.

```Fish Shell
less minfil.txt
```

Ved å trykke på piltastene opp og ned, kan du navigere gjennom filen. For å avslutte, kan du trykke på "q" på tastaturet ditt.

## Deep Dive

Hvis du vil lese en fil som har flere sider med innhold, kan du bruke `head` og `tail` kommandoene. `head` vil vise de første linjene i filen, mens `tail` vil vise de siste linjene.

```Fish Shell
head minfil.txt
```

Dette vil gi oss følgende output:

```
Hei! Velkommen til min blogg om Fish Shell.
```

Mens denne kommandoen:

```Fish Shell
tail minfil.txt
```

Vil gi oss følgende output:

```
Hei! Velkommen til min blogg om Fish Shell.
```

## Se også

- [Offisiell dokumentasjon for Fish Shell](https://fishshell.com/docs/current/)
- [En introduksjon til Fish Shell](https://medium.com/better-programming/a-gentle-introduction-to-the-fish-shell-1577c8af905e)
- [Fish Shell tutorial på norsk](https://github.com/SindreSvendby/fish-tutorial/blob/master/tutorial.md)