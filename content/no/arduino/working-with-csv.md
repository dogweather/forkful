---
title:                "Arbeid med csv"
html_title:           "Arduino: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

# Hvorfor
CSV, eller "Comma Separated Values", er en praktisk filformat som brukes for å lagre og transportere data i en enkel og strukturert måte. Det kan være nyttig for å lagre store mengder data på en organisert måte, og kan også være lett å håndtere og lese for både mennesker og datamaskiner.

# Hvordan 
For å arbeide med CSV-filer ved hjelp av en Arduino, kan du bruke et bibliotek som heter "CSVReader". Dette biblioteket gjør det enkelt å lese og behandle data fra en CSV-fil. Først må du installere biblioteket i ditt Arduino-IDE ved å gå til "Verktøy" og så "Biblioteker Administrere biblioteker". Søk etter "CSVReader" og installer det. Etter at biblioteket er installert, er du klar til å kode!

For å lese en CSV-fil, trenger du først å opprette en instans av "CSVReader" og angi navnet på filen du vil lese som parameter. Her er et eksempel på hvordan dette kan gjøres:

```
#include <CSVReader.h>
CSVReader csvReader("data.csv"); // data.csv er navnet på CSV-filen vi vil lese

void setup() {
  Serial.begin(9600);
}

void loop() {
  String row = csvReader.readRow();
  Serial.println(row); // printer ut en rad av data fra CSV-filen
  delay(500); // venter 500 millisekunder før den leser neste rad
}
```

Etter at du har opprettet en instans av "CSVReader" og har en for-løkke, kan du bruke funksjoner som "readRow()" og "readItem()" for å lese data fra filen. Disse funksjonene returnerer henholdsvis en hel rad av data og et spesifikt element i raden.

# Dypdykk
Når du leser data fra en CSV-fil ved hjelp av Arduino, er det viktig å forstå at dataene blir behandlet som tekst og ikke tall. Dette betyr at du må konvertere dataene til riktig format hvis du ønsker å bruke dem i en beregning eller sammenligning.

En annen viktig ting å merke seg er at CSV-filer kan inneholde mange forskjellige formateringsbrikker og kan være følsomme for hvilken type komma-tegn som brukes. Det kan derfor være lurt å sjekke filen nøye for å sikre at dataene blir lest på riktig måte.

# Se også
- [CSVReader biblioteket](https://github.com/jfsleroy/CSVReader)
- [Offisiell dokumentasjon for Arduino](https://www.arduino.cc/reference/en/)
- [Hvordan installere biblioteker i Arduino IDE](https://www.arduino.cc/en/Guide/Libraries)