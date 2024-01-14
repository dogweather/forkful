---
title:    "Arduino: Kontrollerer om en mappe eksisterer"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Hvorfor

Det å kontrollere om en katalog eksisterer kan være viktig for å sikre at informasjon blir lagret og organisert riktig. Dette er spesielt relevant for Arduino-prosjekter som krever lagring av data.

# Hvordan

Å sjekke om en katalog eksisterer på Arduino kan gjøres ved hjelp av funksjonen `SD.exists()`. Dette står for "SD-kort eksisterer" og returnerer enten `true` eller `false` avhengig av om katalogen finnes. Her er et eksempel på hvordan dette kan brukes sammen med en `if`-setning:

```Arduino
if (SD.exists("/katalog/navn")) {
    Serial.println("Katalogen eksisterer!");
} else {
    Serial.println("Katalogen eksisterer ikke.");
}
```

Et annet eksempel er å bruke funksjonen i en løkke for å søke gjennom flere kataloger og handle deretter:

```Arduino
for (int i=1; i<=5; i++) {
    String katalogNavn = "/prosjekt" + String(i);
    if (SD.exists(katalogNavn)) {
        // gjør noe med dataene i katalogen
    }
}
```

Hvis katalognavnene som skal sjekkes er lagret som variabler, må disse konverteres til en `char`-pekere for å kunne brukes i `SD.exists()`-funksjonen. Dette kan gjøres ved å bruke `strncpy()`-funksjonen.

# Dypdykk

Det er viktig å huske på noen viktige ting når man sjekker om en katalog eksisterer på Arduino-enheten. Først og fremst er det viktig å huske at SD-kortet og kataloger må initialiseres før man kan bruke `SD.exists()`-funksjonen. Dette kan gjøres ved å bruke `SD.begin()` i `setup()`-funksjonen.

Det er også viktig å merke seg at denne funksjonen bare kan sjekke om eksisterende kataloger på det øverste nivået. Dette betyr at man ikke kan bruke den til å sjekke om underkataloger eksisterer.

Til slutt er det viktig å merke seg at denne funksjonen sjekker om katalogen eksisterer på SD-kortet, og ikke nødvendigvis på selve enheten. Dette betyr at katalogen kan bli slettet eller endret uten at `SD.exists()`-funksjonen rapporterer det.

# Se også

- [SD-kort bibliotek for Arduino](https://www.arduino.cc/en/Reference/SD)
- [Arduino dokumentasjon for `SD.exists()`](https://www.arduino.cc/reference/en/libraries/sd/sdfat/sdexists/)