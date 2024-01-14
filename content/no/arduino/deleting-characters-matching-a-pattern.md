---
title:                "Arduino: Sletting av tegn som samsvarer med et mønster"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Å slette tegn som matcher et mønster kan være nyttig når du jobber med å behandle og analysere tekst informasjon. Dette kan inkludere å fjerne uønsket eller ugyldig informasjon, eller å filtrere ut bestemte ord eller setninger.

## Hvordan
Å slette tegn som matcher et mønster i Arduino er enkelt med innebygde funksjoner som `replace()` og `remove()`. Her er et eksempel på hvordan du kan bruke disse funksjonene for å fjerne alle siffer fra en tekststreng og deretter skrive ut resultatet:

```Arduino
String tekst = "Lorem ipsum 123 dolor 456 sit amet";
tekst.replace("123", "");
tekst.replace("456", "");
Serial.println(tekst); // Utskrift: "Lorem ipsum dolor sit amet"
```

For å slette alle tegn som ikke er bokstaver, tall eller mellomrom, kan du bruke en if-setning og `remove()`-funksjonen:

```Arduino
String tekst = "Lorem ipsum 123? dolor 456! sit amet";
for (int i = 0; i < tekst.length(); i++) {
  if (!isAlphaNumeric(tekst.charAt(i)) && tekst.charAt(i) != ' ') {
    tekst.remove(i);
  }
}
Serial.println(tekst); // Utskrift: "Lorem ipsum 123 dolor 456 sit amet"
```

Det finnes også andre funksjoner som kan hjelpe deg med å slette tegn basert på bestemte kriterier, som for eksempel `substring()` og `trim()`. Det er viktig å merke seg at disse funksjonene vil endre den originale tekststrengen, så det kan være lurt å opprette en kopi av teksten før du begynner å slette tegn.

## Dypdykk
Arduino har et stort utvalg av innebygde funksjoner som gjør det enkelt å manipulere tekststrenger. I tillegg kan du også bruke regex (regular expressions) for å definere mer komplekse mønstre som skal slettes. Dette åpner for flere muligheter når det kommer til å behandle og analysere tekstinformasjon.

Du kan også utforske forskjellige måter å slette tegn på ved å kombinere forskjellige funksjoner og metoder. Det er viktig å teste koden nøye for å sikre at den fungerer som ønsket og ikke endrer uønsket informasjon.

## Se også
- [`replace()`-dokumentasjon](https://www.arduino.cc/reference/en/language/variables/string/functions/replace/)
- [`remove()`-dokumentasjon](https://www.arduino.cc/reference/en/language/variables/string/functions/remove/)
- [Regex-tutorial på Norsk](https://www.regular-expressions.info/tutorial.html)