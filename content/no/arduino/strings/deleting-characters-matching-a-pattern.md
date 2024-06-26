---
date: 2024-01-20 17:41:25.213230-07:00
description: "Slik gj\xF8r du: Eksempel utdata: `Hll Wrld! `."
lastmod: '2024-04-05T22:37:49.398850-06:00'
model: gpt-4-1106-preview
summary: 'Eksempel utdata: `Hll Wrld! `.'
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## Slik gjør du:
```Arduino
void setup() {
  Serial.begin(9600);
  String input = "H3ll0 W0rld! 123";
  String pattern = "0-9";
  String output = deletePattern(input, pattern);
  Serial.println(output);
}

void loop() {
  // Ingenting her for nå.
}

String deletePattern(String str, const String& pattern) {
  for (int i = 0; i < pattern.length(); i += 3) { // Antar et enkelt tegnområde '0-9'
    for (int j = 0; j < str.length(); j++) {
      if (str[j] >= pattern[i] && str[j] <= pattern[i+2]) {
        str.remove(j, 1);
        j--; // Juster indeks etter fjerning
      }
    }
  }
  return str;
}
```
Eksempel utdata: `Hll Wrld! `

## Dypdykk
Sletting av tegn etter mønster i Arduino er mer et manuelt inngrep enn i språk med innebygget regular expressions-støtte. Historisk sett var ikke Arduinoer ment for avansert tekstbehandling, men med popularisering og kraftigere maskinvare har behovet økt. Et alternativ til denne manuelle metoden er å bruke en bibliotek som `regex.h`, men dette kan være minnekrevende. Implementasjonen over er grunnleggende og er ment for enkle mønstre, som å fjerne sifre eller spesifikke tegn.

## Se også
- Arduino String Class Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- `regex.h` bibliotek for mer avanserte mønster-slettingsscenarier: https://github.com/nickgammon/Regexp
- Guide til effektiv strengmanipulering i Arduino: https://www.baldengineer.com/arduino-string-manipulation-tips.html
