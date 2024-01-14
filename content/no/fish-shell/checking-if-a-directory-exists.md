---
title:                "Fish Shell: Sjekke om en mappe eksisterer"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på om en mappe eksisterer i Fish Shell-programmering? Det er flere gode grunner til hvorfor du kanskje trenger å sjekke dette. Kanskje du vil utføre en spesifikk handling bare hvis mappen eksisterer, eller kanskje du vil unngå å overskrive en eksisterende mappe. Uansett årsak, hvis du trenger å sjekke om en mappe eksisterer, er det viktig å vite hvordan du gjør det riktig.

## Hvordan

Fish Shell har en innebygd kommando, `test`, som kan brukes til å sjekke om en fil eller mappe eksisterer. Du kan bruke følgende syntaks for å sjekke om en mappe eksisterer i Fish Shell:

```Fish Shell
if test -d <mappenavn>
    echo "Mappen eksisterer"
else
    echo "Mappen eksisterer ikke"
end
```
Dette vil sjekke om mappen med navnet du har gitt eksisterer. Hvis den gjør det, vil den skrive ut "Mappen eksisterer", ellers vil den skrive ut "Mappen eksisterer ikke".

## Deep Dive

Kommandoen `test` i Fish Shell tar en rekke argumenter, og i eksempelet over brukte vi `-d` for å sjekke om en mappe eksisterer. Her er noen andre nyttige argumenter som kan brukes for å sjekke filer og mapper:

- `-e`: Sjekker om filen eller mappen eksisterer.
- `-f`: Sjekker om det er en vanlig fil.
- `-L`: Sjekker om det er en symbolisk link.
- `-r`: Sjekker om filen eller mappen er lesbar.
- `-w`: Sjekker om filen eller mappen er skrivbar.
- `-x`: Sjekker om filen eller mappen er utførbar.

Hvis du vil lære mer om disse argumentene og andre funksjoner i `test`-kommandoen, kan du sjekke ut Fish Shell sin dokumentasjon.

## Se også

- [Fish Shell sin offisielle dokumentasjon](https://fishshell.com/docs/current/cmds.html#test)
- [En guide til å programmere i Fish Shell](https://codeburst.io/getting-started-with-fish-shell-f1c4c8ec6059)
- [Fish Shell tips og triks for nybegynnere](https://medium.com/@crmaxstudio/fish-the-shell-of-the-future-tips-tricks-for-beginners-847cf85ed8be)