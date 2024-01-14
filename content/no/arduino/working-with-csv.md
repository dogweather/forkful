---
title:                "Arduino: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med CSV-filer kan være en nyttig ferdighet å ha for alle som bruker Arduino. CSV står for "Comma Separated Values" og er en enkel og vanlig format for å lagre og transportere data. Ved å kunne lese og manipulere CSV-filer, kan du enkelt lagre og hente ut data fra sensorer og andre enheter som er koblet til Arduinoen din.

## Hvordan

Det første du trenger å gjøre er å installere biblioteket "Arduino CSV" ved å følge disse trinnene:

1. Åpne Arduino IDE
2. Gå til "Verktøy" og velg "Håndter bibliotek"
3. Søk etter "Arduino CSV" og klikk på "Installer"
4. Etter installasjonen, start IDEen på nytt for å aktivere biblioteket.

Nå kan du bruke følgende kodeeksempel for å lese en CSV-fil og skrive ut dataene til seriellmonitoren:

```
#include <CSV.h>

void setup() {
  Serial.begin(9600);
  CSV csv;
  csv.open("data.csv");
  while (csv.getNextRow()) {
    Serial.print(csv[0]); // Den første verdien i hver rad
    Serial.print(", ");
    Serial.print(csv[1]); // Den andre verdien i hver rad
    Serial.println();
  }
  csv.close();
}

void loop() {

}
```

På samme måte kan du bruke biblioteket til å skrive data til en CSV-fil. For å gjøre dette, bruk `addRow()` -metoden etterfulgt av `writeFile()` for å lagre fila. Her er et eksempel:

```
#include <CSV.h>

void setup() {
  Serial.begin(9600);
  CSV csv;
  csv.addRow("Temperatur", "Luftfuktighet"); // Legg til kolonnenavn
  csv.addRow(25, 60); // Legg til data
  csv.addRow(23, 55);
  csv.writeFile("data.csv"); // Lagre fila
}

void loop() {

}
```

Du kan også bruke `setDelimiter()` -metoden for å endre skilletegnet mellom verdiene i fila til hva som helst passende for ditt prosjekt.

## Dypdykk

CSV-biblioteket har flere funksjoner som lar deg hente ut og manipulere data fra CSV-filer. Du kan for eksempel bruke `getColumn()` -metoden for å få ut en hel kolonne med data, eller `getRows()` -metoden for å få ut en spesifikk rekke med data. For å få mer informasjon om disse og andre funksjoner, kan du se på dokumentasjonen til biblioteket.

En ting å være obs på når du jobber med CSV-filer er å sørge for at filen du prøver å lese eller skrive til har riktig formatering. Hvis det er feil i formateringen, vil biblioteket ikke kunne lese dataene riktig. Som en tommelfingerregel, sørg for å alltid ha samme antall verdier på hver rad og at verdier separeres med riktig skilletegn.

## Se også

- [Arduino CSV bibliotek dokumentasjon](https://github.com/arduino-libraries/Arduino_CSV)
- [Tutorial on using CSV files with Arduino](https://circuitdigest.com/microcontroller-projects/using-csv-files-to-plot-data-in-arduino-projects)
- [CSV file guide for beginners](https://docs.python.org/3/library/csv.html) (for Python, men konseptene gjelder også for Arduino)