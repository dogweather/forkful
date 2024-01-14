---
title:                "Fish Shell: Å skrive en tekstfil"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en viktig del av programmeringsverdenen. Det lar deg lagre og organisere informasjon på en strukturert måte. Med Fish Shell, en populær og brukervennlig kommandolinje, kan du enkelt og raskt opprette tekstfiler.

## Hvordan

For å opprette en tekstfil i Fish Shell, kan du følge disse enkle trinnene:

1. Åpne terminalen og naviger til ønsket mappe.
2. Skriv inn kommandoen ```touch filnavn.txt``` for å opprette en tom tekstfil med navnet du ønsker.
3. Skriv inn kommandoen ```echo "Tekst du ønsker å legge til i filen" >> filnavn.txt``` for å legge til tekst i filen.
4. For å se innholdet i filen, skriv inn ```cat filnavn.txt```.

## Deep Dive

Fish Shell har også flere nyttige funksjoner for tekstbehandling. En av dem er muligheten til å tilordne variabler i en tekstfil. Dette kan gjøres ved å bruke kommandoen ```set variabelnavn = verdi``` og deretter bruke variabelnavnet i filen ved hjelp av ```$variabelnavn```.

En annen nyttig funksjon er å telle antall linjer, ord og tegn i en tekstfil. Dette kan gjøres ved å bruke kommandoen ```wc -l ordnavn.txt``` for å telle antall linjer, og tilsvarende for ord og tegn ved å bruke ```wc -w``` og ```wc -c```.

## Se også

- [Fish Shell nettside](https://fishshell.com/)
- [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Fish Shell på GitHub](https://github.com/fish-shell/fish-shell)