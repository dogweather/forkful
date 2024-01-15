---
title:                "Kontrollere om en mappe eksisterer"
html_title:           "Haskell: Kontrollere om en mappe eksisterer"
simple_title:         "Kontrollere om en mappe eksisterer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Det er viktig å vite om en mappe eksisterer eller ikke i Haskell-programmering, spesielt når man arbeider med filbehandling. Dette kan være nyttig for å unngå feil under kjøring og for å sørge for at programmet fungerer som forventet.

# Slik gjør du det

```Haskell
import System.Directory

checkDirectory :: FilePath -> IO Bool
checkDirectory path = doesDirectoryExist path
```

I dette eksempelet importerer vi funksjonen ```doesDirectoryExist``` fra modulen ```System.Directory```. Deretter bruker vi denne funksjonen i ```checkDirectory```-funksjonen vår for å sjekke om en mappe med en spesifikk filsti eksisterer. Funksjonen returnerer en ```IO Bool```, som indikerer om mappen eksisterer eller ikke. 

```Haskell
main :: IO ()
main = do
  print "Sjekker om mappen eksisterer..."
  exist <- checkDirectory "min_mappe"
  if exist
    then print "Mappen eksisterer."
    else print "Mappen eksisterer ikke."
```

I dette eksempelet viser vi hvordan vi kan bruke ```checkDirectory```-funksjonen i et faktisk program. Vi bruker ```do```-blokk for å utføre handlingene våre ved kjøring av programmet. Vi sjekker om mappen "min_mappe" eksisterer, og hvis den gjør det, skriver vi ut en melding om at den eksisterer. Hvis ikke, skriver vi ut en annen melding. 

# Dypdykk

Når du bruker funksjonen ```doesDirectoryExist```, er det viktig å merke seg at den sjekker om mappen eksisterer på det nåværende tidspunktet. Hvis du for eksempel sjekker om en mappe eksisterer, og deretter oppretter den i samme program, vil funksjonen returnere ```False``` fordi mappen ikke eksisterte når den ble sjekket. 

En annen viktig ting å huske på er at ```doesDirectoryExist``` bare sjekker om mappen eksisterer, ikke om det er en vanlig mappe eller en fil. Det er derfor viktig å kontrollere hva slags fil det er før du prøver å behandle den videre. 

# Se også

- [Dokumentasjon for System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell filbehandling](https://wiki.haskell.org/Handling_files)