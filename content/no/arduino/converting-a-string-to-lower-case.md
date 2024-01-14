---
title:    "Arduino: Konvertere en streng til små bokstaver"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Hvorfor

Når du utvikler et prosjekt på Arduino-plattformen, kan du noen ganger komme over behovet for å konvertere en tekststreng til små bokstaver. Dette kan være nyttig hvis du for eksempel ønsker å sammenligne to strenger uten at forskjellen i store og små bokstaver påvirker resultatet. I denne artikkelen vil jeg vise deg hvordan du kan konvertere en tekststreng til små bokstaver i Arduino-programmering.

# Hvordan gjøre det

For å konvertere en streng til små bokstaver i Arduino, kan du bruke den innebygde funksjonen "toLowerCase()". Denne funksjonen tar inn en tekststreng som parameter og returnerer en ny streng med alle bokstavene i små bokstaver.

```Arduino
String tekst = "HEI, JEG ER EN ARDUINO PROGRAMMERER";

String konvertertTekst = tekst.toLowerCase();

Serial.println(konvertertTekst); // Gir ut "hei, jeg er en arduino programerer"
```

Som du kan se i eksemplet over, har vi lagret den originale strengen "HEI, JEG ER EN ARDUINO PROGRAMMERER" i en variabel kalt "tekst". Deretter har vi brukt "toLowerCase()" -funksjonen til å konvertere denne strengen til små bokstaver og lagre den i en ny variabel kalt "konvertertTekst". Til slutt skriver vi ut denne konverterte strengen til seriell monitor ved hjelp av "Serial.println()" -funksjonen.

# Deep Dive

Hvis du ønsker å forstå mer om hvordan "toLowerCase()" -funksjonen fungerer under overflaten, kan du ta en titt på kildekoden for denne funksjonen. Naviger til "Arduino\hardware\arduino\avr\cores\arduino\WString.cpp" for å finne kildekoden. Der kan du se at funksjonen egentlig bare går gjennom alle bokstavene i den originale strengen og endrer hver enkelt bokstav til små bokstaver ved hjelp av ASCII-kode konvertering.

# Se også

- [Arduino String klasse referanse](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ASCII kode tabell](https://www.ascii-code.com/)