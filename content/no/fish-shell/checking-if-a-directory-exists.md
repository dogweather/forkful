---
title:                "Sjekke om en katalog eksisterer"
html_title:           "Fish Shell: Sjekke om en katalog eksisterer"
simple_title:         "Sjekke om en katalog eksisterer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe eksisterer, handler om å bekrefte at en bestemt mappe (eller katalog) finnes i filsystemet før du foretar deg noe med den. Programmerere gjør dette for å forhindre feil som kan oppstå når du prøver å manipulere en ikke-eksisterende mappe.

## Hvordan:
Her er hvordan du sjekker om en mappe eksisterer i Fish Shell.

```Fish Shell
if test -d /sti/til/mappen
    echo "Mappen eksisterer."
else
    echo "Mappen eksisterer ikke."
end
```
Utfør koden ovenfor, så hvis mappen eksisterer, vil utdata være "Mappen eksisterer.". Hvis ikke, "Mappen eksisterer ikke.".

## Dypdykk
Tidligere versjoner av Fish Shell støttet ikke denne funksjonaliteten rett ut av boksen, så du måtte ty til å bruke andre verktøy som `find` eller `ls` for å oppnå det samme. Selv om alternativer som `find` og `ls` fortsatt kan brukes i noen tilfeller, er `test -d` mer pålitelig og mindre tilbøyelig til å lage feil, spesielt med mappenavn som inneholder spesielle tegn.

Undertestet `-d` fungerer ved å spørre operativsystemet direkte om mappen eksisterer, noe som er både raskere og mer pålitelig.

## Se også
For mer informasjon om å kode i Fish Shell, sjekk ut [Fish Shell dokumentasjonen](https://fishshell.com/docs/current/index.html). Du kan også finne [dette blogginnlegget](https://jorgebucaran.com/understanding-test-in-fish/) om 'understanding test in fish' nyttig for mer avanserte bruksscenarioer.