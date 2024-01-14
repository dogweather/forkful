---
title:                "Haskell: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV (Comma Separated Values) er en svært vanlig filformat brukt for å lagre og utveksle tabelløs data. Det kan være data som kommer fra en database eller et regneark, og CSV-filer kan enkelt åpnes og redigeres med tekstbehandlingsprogrammer som Excel eller Google Sheets. Å kunne arbeide med CSV-filer er en viktig ferdighet for enhver utvikler eller dataanalytiker, da det kan hjelpe med å manipulere, bearbeide og analysere store mengder data på en effektiv måte.

## Hvordan

For å jobbe med CSV-filer i Haskell, trenger du å importere "cassava" biblioteket ved å skrive følgende i toppen av filen din:

```Haskell
import Cassava
```
Deretter kan du lese inn en CSV-fil ved hjelp av "Cassava.parseFile" funksjonen, og spesifisere typen til dataene du vil lese inn. For eksempel:

```Haskell
import Cassava
main = do
  records <- Cassava.parseFile "data.csv" :: IO (Cassava.Vector (T.Text, Int))
  print records
```

I dette eksempelet vil CSV-filen inneholde data i formatet "Tekst, Heltall". Ved å bruke "parseFile" funksjonen, vil dataene bli lest inn som en "Cassava.Vector" type som kan bli skrevet ut ved hjelp av "print" funksjonen.

## Deep Dive

Når du leser inn en CSV-fil, vil hver rad bli representert som en rekord i "Cassava.Vector". For å få tilgang til spesifikke data fra en rad, kan du bruke indeksering. For eksempel, for å få tilgang til tredje element i første rad, kan du skrive:

```Haskell
thirdelement = records !! 0 !! 2 
```

I tillegg til å lese inn en CSV-fil, kan du også skrive data til en CSV-fil ved hjelp av "Cassava.encodeFile" funksjonen. Dette vil tillate deg å formatere dataene dine og skrive dem til en ny CSV-fil.

For mer detaljert informasjon om hvordan du arbeider med CSV-filer i Haskell, kan du sjekke ut "Cassava" bibliotekets dokumentasjon.

## Se også

- "Cassava" dokumentasjon: https://hackage.haskell.org/package/cassava/docs/Data-Csv.html
- Guide til å jobbe med CSV-filer i Haskell: https://www.snoyman.com/blog/2017/05/beware-the-high-cost-of-default-strings 
- Eksempler på å arbeide med CSV-filer i Haskell: https://github.com/haskell-art/awesome-haskell/blob/master/csv/readme.md

Takk for at du tok deg tid til å lese denne artikkelen om å arbeide med CSV-filer i Haskell. Vi håper det vil hjelpe deg med å lære denne viktige ferdigheten og gjøre deg til en bedre utvikler eller dataanalytiker. Lykke til med kodingen!