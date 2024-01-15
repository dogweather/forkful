---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Arduino: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor
Det er viktig å vite hvordan man kan sjekke om en mappe eksisterer når man jobber med Arduino-prosjekter. Dette kan hjelpe til med å organisere filene dine og sikre at koden kjører riktig.

# Slik gjør du det
For å sjekke om en mappe eksisterer, kan du bruke funksjonen `exists()` fra SD-biblioteket på Arduino. Dette vil returnere `true` eller `false` avhengig av om mappen finnes eller ikke.

```
Arduino SD-bibliotekAPI
=============

**exists()**

Beskrivelse:
Sjekker om en mappe finnes.

Sintaks:
if (SD.exists(mappenavn)) {
  // gjør noe hvis mappen finnes
} else {
  // gjør noe hvis mappen ikke finnes
}

Eksempel:

// Sjekker om mappen "dokumenter" eksisterer
if (SD.exists("dokumenter")) {
  Serial.println("Mappen finnes!");
} else {
  Serial.println("Mappen finnes ikke!");
}

Resultat:
Mappen finnes ikke!
```

# Dypdykk
`exists()` funksjonen bruker `open()` metoden i SD-biblioteket for å åpne filen. Hvis filen ikke eksisterer, vil metoden returnere `false`, og dermed returnerer også `exists()` funksjonen `false`.

Hvis du ønsker å sjekke om en fil eksisterer i en undermappe, kan du bruke denne syntaksen: `SD.exists("/undermappe/navn")`.

Hvis du vil sjekke om SD-kortet er satt inn og klar til å brukes, kan du bruke `SD.begin()` funksjonen og sjekke returverdien. Hvis `SD.begin()` returnerer `1`, betyr det at SD-kortet er klart til å brukes.

# Se Også
- [SD-bibliotekets offisielle dokumentasjon](https://www.arduino.cc/en/Reference/SD)
- [Hvordan bruke SD-kort med Arduino](https://create.arduino.cc/projecthub/detroyer/how-to-use-sd-card-with-arduino-25e4ad)