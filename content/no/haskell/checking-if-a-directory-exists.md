---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Haskell: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sjekke om en katalog eksisterer refererer til handlingen å bekrefte om en spesifikk katalog fins på varslet plass i lagringssystemet. Programmerere gjør dette for å hindre feil som kan oppstå når man prøver å få tilgang til en katalog som ikke eksisterer.

## Hvordan:

Her er hvordan du kan sjekke om en katalog eksisterer i Haskell, ved hjelp av `System.Directory` pakken.

```Haskell
import System.Directory

main = do
    let dirName = "/sti/til/katalog"
    dirExists <- doesDirectoryExist dirName
    print dirExists
```

Kodeutdraget ovenfor vil returnere `True` dersom katalogen eksisterer, og `False` dersom den ikke gjør det.

## Dypdykk

Haskell's innebygde funksjon `doesDirectoryExist` har vært tilgjengelig siden starten av `System.Directory` modulen rundt 2001. 

Det finnes også alternative metoder for å sjekke om en katalog eksisterer. En er ved å bruke `catchIOError` funksjonen som tillater behovsbetinget håndtering av mulige I/O feil, inklusive manglende kataloger.

Hvis du vil dykke dypere i hvordan `doesDirectoryExist` funksjonen er implementert, kan du undersøke Haskell's bibliotekskildekode. Men i en nøtteskall, bruker det de underliggende systemkallene for å interagere med filsystemet.

## Se Også

- [Haskell System.Directory Modul Dokumentasjon](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)