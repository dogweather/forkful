---
title:                "Skriver en tekstfil"
html_title:           "Haskell: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Å skrive en tekstfil er en måte for programmerere å lagre informasjon på en permanent måte. Dette betyr at dataene vil bli lagret selv når programmet er avsluttet eller enheten er slått av. Dette er nyttig for å lagre viktige data eller resultater fra et program.

# Hvordan:
I Haskell kan du enkelt skrive en tekstfil ved å bruke funksjonen `writeFile`. Denne funksjonen tar inn en filbane og en streng med tekst som argumenter, og skriver strengen til den angitte filbanen. Her er et eksempel:

```Haskell
writeFile "minfil.txt" "Dette er teksten som vil bli lagret i filen."
```

Etter å ha kjørt denne koden, vil en fil med navnet "minfil.txt" bli opprettet i den samme mappen som Haskell-koden blir kjørt fra. Hvis filen allerede eksisterer, vil innholdet i den bli overskrevet.

# Dypdykk:
Å kunne skrive tekstfiler har vært en viktig del av programmering i lang tid. Før digitale datamaskiner ble vanlig, ble data lagret på hullkort eller teip som inneholdt perforerte hull. Tekstfiler er en digital versjon av dette konseptet og er fortsatt i stor bruk i dag.

Haskell har også flere alternative måter å skrive tekstfiler på, som for eksempel funksjonen `appendFile`, som lar deg legge til tekst til en eksisterende fil i stedet for å overskrive den. Det finnes også ulike biblioteker som tilbyr mer avanserte måter å håndtere tekstfiler på, som for eksempel å lese og skrive CSV-filer.

Når det kommer til implementasjonen av hvordan Haskell lagrer tekstfiler, er det avhengig av hvilket operativsystem du bruker. Generelt sett vil Haskell følge standarden til operativsystemet for å håndtere filen. Dette betyr at linjeskift kan være annerledes mellom f.eks. Windows og Linux.

# Se også:
- [Dokumentasjon for `writeFile`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:writeFile)
- [Alternativ til `writeFile`: `Data.Text.IO.writeFile`](https://hackage.haskell.org/package/text/docs/Data-Text-IO.html#v:writeFile)
- [Lese og skrive CSV-filer i Haskell](https://www.snoyman.com/blog/2016/12/beware-csv)