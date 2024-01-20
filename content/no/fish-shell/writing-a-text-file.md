---
title:                "Skriving av en tekstfil"
html_title:           "Fish Shell: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Hva & Hvorfor?
Å skrive en tekstfil er en grunnleggende programmeringsaktivitet hvor man lager en fil med tekst og kode som modellen av programmet ditt bruker for å utføre en bestemt funksjon. Programmere gjør dette for å organisere og lagre sin kode på en strukturert og lesbar måte.

##Slik gjør du det:
```Fish Shell``` brukes ofte til å skrive tekstfiler enkelt og effektivt. For å lage en tekstfil i Fish Shell, følg disse trinnene:
1. Åpne terminalen og naviger til ønsket mappe ved å skrive ```cd mappe```
2. Skriv deretter kommandoen ```touch filnavn.txt``` for å opprette en tom tekstfil med navnet du ønsker.
3. For å legge inn tekst i filen, skriv kommandoen ```echo "Din tekst her" >> filnavn.txt```. Dette vil legge til teksten "Din tekst her" i filen din.
4. For å sjekke at teksten er lagt til, kan du bruke kommandoen ```cat filnavn.txt```, som vil vise innholdet av filen i terminalen.

##Dykk dypere:
Skriving av tekstfiler har eksistert siden de tidlige dagene av datamaskiner, og har vært en viktig del av programmering. Det finnes flere andre metoder for å skrive tekstfiler i andre programmeringsspråk, som for eksempel ```notepad``` i Windows eller ```vi``` i Linux.

Etter å ha opprettet en tekstfil, kan du endre filens tillatelser ved å bruke kommandoen ```chmod```. Dette er nyttig for å gi andre brukere rettigheter til å lese, skrive eller utføre filen din.

##Se også:
Få mer informasjon om Fish Shell og tekstbehandling i terminalen ved å sjekke ut disse kildene:
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [chmod kommandoen](http://man7.org/linux/man-pages/man1/chmod.1.html)