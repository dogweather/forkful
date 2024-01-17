---
title:                "Arbeid med csv"
html_title:           "Haskell: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV (Comma Separated Values) er et vanlig filformat for å lagre og utveksle tabellariske data. Dette kan være alt fra en enkel liste med navn og e-postadresser til komplekse finansielle data. Programmerere jobber med CSV for å kunne effektivt håndtere og manipulere store mengder data, og for å kunne analysere og visualisere data på en strukturert og enkel måte.

## Hvordan:
For å jobbe med CSV i Haskell, trenger du å importere Data.CSV-modulen. Deretter kan du bruke funksjonene ```parseCSV``` og ```printCSV``` for å lese og skrive CSV-filer. Her er et enkelt eksempel på hvordan du kan lese en CSV-fil og skrive ut innholdet:

```Haskell
import Data.CSV

main = do
    csvData <- parseCSVFromFile "data.csv" -- "data.csv" er navnet på filen du ønsker å lese
    case csvData of
        Left err -> putStrLn "Feil ved parsing av filen"
        Right csv -> print csv
```

Eksempel på utoutput:
```
[["Navn","E-post"],["Ole","ole@eksempel.no"],["Kari","kari@eksempel.no"],["Per","per@eksempel.no"]]
```

## Dypdykk:
CSV ble utviklet på 1970-tallet som et enkelt og åpent filformat for å lagre og utveksle data mellom forskjellige dataprogrammer. Et alternativ til å jobbe med CSV kan være å bruke XML eller JSON som også er vanlige filformater for å lagre og utveksle data. Implementasjonen av CSV i Haskell er basert på RFC 4180-standard, som spesifiserer det formelle formatet for CSV-filer.

## Se også:
- [Data.CSV-modulen i Haskell standard library](https://hackage.haskell.org/package/csv/docs/Data-CSV.html)
- [RFC 4180: Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://tools.ietf.org/html/rfc4180)
- [CSV vs XML vs JSON: Which is the best format for storing data? (Artikkel)](https://www.educba.com/csv-vs-xml-vs-json/)