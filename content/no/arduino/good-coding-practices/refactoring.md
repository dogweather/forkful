---
title:                "Refaktorering"
aliases:
- /no/arduino/refactoring/
date:                  2024-01-26T01:16:45.741107-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/refactoring.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
*Refactoring* er prosessen med å omarbeide koden din for å forbedre strukturen og lesbarheten uten å endre den eksterne oppførselen eller funksjonaliteten. Programmerere refaktorerer for å gjøre koden sin renere, lettere å forstå og mer vedlikeholdbar, noe som på lang sikt gjør feilsøking og legging til nye funksjoner langt mindre hodepine.

## Hvordan:

La oss si du har en funksjon på din Arduino som gjør altfor mye, slik som dette:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // En funksjon som gjør for mye
  handleEverything();
}

void handleEverything() {
  // Les sensordata
  int sensorValue = analogRead(A0);
  // Behandle sensordata
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Skriv ut sensordata
  Serial.println(sensorValue);
  delay(500);
}
```

Å refaktorere det, kan se ut som å splitte `handleEverything()` inn i mindre, mer fokuserte funksjoner:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Etter refaktoreringen er `loop()` funksjonen mer lesbar, og hver oppgave håndteres av en dedikert funksjon, noe som gjør koden enklere å administrere.

## Dypdykk
Historisk sett ble refaktorering populært med oppgangen til Agile og testdrevet utvikling (TDD) metodologier, som stoler på konstant kodeforbedring for å tilpasse seg endrende krav. Det finnes ulike verktøy og strategier for refaktorering – som "Extract Method"-teknikken vi brukte i vårt Arduino-eksempel. Dette er essensielt når du går fra en rask prototype til et stabilt prosjekt, der kodelesbarhet og vedlikehold blir avgjørende.

Når du refaktorerer, er det viktig å ha et godt sett med tester på plass for å sikre at endringene ikke har introdusert noen feil. I Arduino-verdenen er ikke automatisert testing alltid rett frem på grunn av maskinvareavhengighetene, men du kan fortsatt bruke enhetstesting for rene logikkbiter eller benytte simulatorer.

Alternativer til manuell refaktorering inkluderer å bruke dedikerte refaktoriseringsverktøy, som automatiserer identifiseringen av kode tvil og foreslår endringer. Imidlertid mangler disse verktøyene ofte nyansene for mikrokontrollerkode og er kanskje ikke tilgjengelige i Arduino-utviklingsmiljøet.

Til slutt er refaktorering en kunst som balanserer forbedring av kodenens interne struktur mot risikoen for å innføre feil. Det krever at du tenker over implementeringsdetaljer som minnebruk og prosessortid, spesielt på grunn av mikrokontrollernes ressursbegrensede natur.

## Se også
Du kan dykke dypere inn i refaktorering med Martin Fowlers grunnleggende bok *Refactoring: Improving the Design of Existing Code*. For et nærmere blikk på Arduino-spesifikke praksiser, sjekk ut Arduino-utviklingsforumer og -fellesskap:
- [Arduino Forum - Programmeringsspørsmål](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Husk, målet er ren, forståelig kode som fremtidige du og andre vil takke deg for. Fortsett å hacke, og hold det ryddig!
