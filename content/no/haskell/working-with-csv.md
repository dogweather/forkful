---
title:                "Arbeide med csv"
html_title:           "Haskell: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er interessert i dataanalyse eller behandling av store mengder data, kan arbeid med CSV-filer være nyttig for deg. CSV står for "Comma-Separated Values" og er et vanlig format for å lagre og organisere data.

## Hvordan

For å kunne jobbe med CSV-filer i Haskell, må vi først importere et modul som håndterer lesing og skriving av CSV-filer. Vi kan gjøre dette ved å legge til følgende linje øverst i koden vår:

```Haskell
import Text.CSV
```

For å lese en CSV-fil og lagre dataene i en liste av lister, kan vi bruke funksjonen `parseCSVFromFile`. Denne tar inn en filbane og returnerer en `IO` handling som kan utføres for å få tilgang til dataene. Her er et eksempel på hvordan dette kan se ut:

```Haskell
main = do
  result <- parseCSVFromFile "sample.csv"
  case result of
    Left err -> putStrLn "Feil ved lesing av fil."
    Right csv -> print csv
```

Output vil være en liste av lister hvor hver indre liste representerer en rad med data fra CSV-filen. Et eksempel på CSV-filen `sample.csv` kan se slik ut:

```
Navn,Alder,Kjønn
Maria,28,Kvinne
Marius,34,Mann
Julie,22,Kvinne
```

Og outputen fra eksempelet over vil da være:

```
[["Navn","Alder","Kjønn"],["Maria","28","Kvinne"],["Marius","34","Mann"],["Julie","22","Kvinne"]]
```

For å skrive data til en CSV-fil kan vi bruke funksjonen `writeFile`. Denne tar inn en filbane og en liste av lister med data, og skriver denne dataen til filen i CSV-format. Her er et eksempel på hvordan dette kan se ut:

```Haskell
main = do
  let csv = [["Frukt", "Antall"], ["Eple", "5"], ["Banan", "10"], ["Appelsin", "3"]]
  writeFile "fruktliste.csv" (printCSV csv)
```

Output vil da bli en fil med navn `fruktliste.csv` som inneholder følgende data:

```
Frukt,Antall
Eple,5
Banan,10
Appelsin,3
```

## Dypdykk

Når du jobber med CSV-filer i Haskell, kan det være lurt å kjenne til noen nyttige funksjoner som hjelper deg med å manipulere og analysere dataene. Her er noen av de mest nyttige funksjonene fra `Text.CSV`-modulen:

- `csvRecord`: Denne funksjonen tar inn en liste med verdier og returnerer en `Record`, som er en type definert i `Text.CSV`-modulen. Dette kan være nyttig når du skal legge til en ny rad i en CSV-fil.
- `readCSV`: Denne funksjonen tar inn en `String` og returnerer en `Either` type som enten inneholder en feilmelding eller en `Record` som er blitt parsert fra `String`-en. Dette kan være nyttig hvis du trenger å parse data som kommer fra et inputfelt i et brukergrensesnitt.
- `encodeBy` og `decodeBy`: Disse funksjonene lar deg definere dine egne regler for hvordan data skal bli kodet og dekodet fra og til CSV-format. Dette kan være nyttig hvis du trenger å håndtere spesielle tegn eller formater i dataene dine.

## Se også

- Offisiell dokumentasjon for `Text.CSV`-modulen: https://hackage.haskell.org/package/csv
- En guide til å jobbe med CSV-filer i Haskell: https://www.datacamp.com/community/tutorials/haskell-working-with-csv-files