---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:01.868856-07:00
description: "\xC5 sette stor bokstav i en streng inneb\xE6rer \xE5 konvertere det\
  \ f\xF8rste tegnet i hvert ord i en streng til stor bokstav mens resten forblir\
  \ sm\xE5 bokstaver.\u2026"
lastmod: '2024-03-13T22:44:41.042241-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sette stor bokstav i en streng inneb\xE6rer \xE5 konvertere det f\xF8\
  rste tegnet i hvert ord i en streng til stor bokstav mens resten forblir sm\xE5\
  \ bokstaver."
title: Sette stor bokstav i en streng
weight: 2
---

## Hva & Hvorfor?
Å sette stor bokstav i en streng innebærer å konvertere det første tegnet i hvert ord i en streng til stor bokstav mens resten forblir små bokstaver. Denne operasjonen er vanlig i dataformatering og normalisering av brukerinndata for å opprettholde konsistens og bedre lesbarhet.

## Hvordan:
Arduino, primært kjent for å samhandle med maskinvare, inkluderer også grunnleggende funksjoner for manipulering av strenger gjennom sitt `String`-objekt. Det mangler imidlertid en direkte `capitalize`-funksjon sett i høyere nivåspråk. Derfor implementerer vi kapitalisering ved å iterere over en streng og anvende transformasjoner av bokstavstørrelse.

Her er et grunnleggende eksempel uten å bruke tredjepartsbibliotek:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Returner en tom streng hvis input er tom
  }
  input.toLowerCase(); // Konverter hele strengen til små bokstaver først
  input.setCharAt(0, input.charAt(0) - 32); // Gjør første tegn til stor bokstav
  
  // Gjør bokstaver til store bokstaver som følger etter et mellomrom
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Utdata: "Hello Arduino World"
}

void loop() {
  // Tom løkke
}
```

Denne kodesnutten definerer en `capitalizeString`-funksjon som først konverterer hele strengen til små bokstaver for å standardisere dens tilfelle. Deretter gjør den det første tegnet og ethvert tegn som følger etter et mellomrom til store bokstaver, noe som effektivt kapitaliserer hvert ord i inndatastrengen. Merk at denne grunnleggende implementeringen antar ASCII-tegnkoding og kan trenge justeringer for full Unicode-støtte.

For tiden er det ikke bredt aksepterte tredjepartsbibliotek spesifikt for strengmanipulering i Arduino-økosystemet, hovedsakelig på grunn av dets fokus på maskinvareinteraksjon og effektivitet. Imidlertid er det gitte eksemplet en grei måte å oppnå kapitalisering av strenger innenfor Arduinos programmeringsmiljø.
