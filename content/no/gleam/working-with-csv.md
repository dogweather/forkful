---
title:                "Arbeide med csv"
html_title:           "Gleam: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å jobbe med CSV, eller "Comma Separated Values", er en måte for programmerere å håndtere store mengder data på en strukturert og effektiv måte. CSV-filer er en vanlig måte å lagre data på, og brukes ofte til å overføre informasjon mellom ulike systemer. Programmere benytter seg av CSV fordi det tillater dem å enkelt lese og manipulere data uten å måtte håndtere kompliserte formater.

## Slik gjør du det:
For å lese en CSV-fil i Gleam, kan du bruke den innebygde ```File.csv``` modulen. Først må filen importeres ved hjelp av ```import File``` kommandoen. Deretter kan du bruke funksjonen ```read``` for å lese filen og lagre dataene som en liste med rader. Hvis CSV-filen inneholder en overskriftsrad, kan du bruke funksjonen ```with_headers``` for å lagre den første raden som overskrifter i en mappe. Her er et eksempel på hvordan du kan lese og skrive ut dataene i en CSV-fil:

```Gleam
import File

let file = File.open("my_csv_file.csv")
let rows = File.csv.read(file) // Leser CSV-filen og lagrer dataene som en liste
let headers_and_data = File.csv.with_headers(rows) // Setter første rad som overskrifter
File.close(file) // Lukke fila når du er ferdig med å bruke den

for row in rows {
  println(row) // Skriver ut hver rad i CSV-filen
}

for {header, data} in headers_and_data {
  println(header ++ ": " ++ data) // Skriver ut hver overskrift sammen med tilhørende data
}
```

## Dykk dypere:
CSV-formatet ble utviklet i 1972 som en enkel måte å lagre data på, og har siden blitt en standard for utveksling av informasjon mellom ulike systemer. Selv om CSV er mye brukt, har det også sine begrensninger. For eksempel støtter det ikke komplekse datastrukturer, som kan gjøre det vanskelig å lagre data på en optimal måte. Alternativer til CSV inkluderer JSON og XML, som begge støtter mer komplekse datastrukturer og er mer utbredt i moderne programmering.

Når du arbeider med CSV i Gleam, bør du være oppmerksom på eventuelle problemer med tegnsett og tegn som kan bryte med CSV-formatet. Det er også viktig å ha en god forståelse av hvordan CSV-dataene dine er strukturert, siden det kan påvirke ytelsen når du leser og manipulerer dataene.