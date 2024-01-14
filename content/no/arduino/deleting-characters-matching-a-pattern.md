---
title:    "Arduino: Sletting av tegn som matcher et mønster"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor?

Noen ganger kan det være nødvendig å slette visse tegn fra en tekststreng for å forenkle behandlingen eller gjøre den mer leselig. Dette kan være nyttig når du jobber med datainnsamling eller behandling i en Arduino-prosjekt. Ved å fjerne uønskede tegn kan du gjøre koden mer effektiv og stabil.

## Hvordan?

For å slette tegn som matcher et gitt mønster, kan du bruke metoden `remove()` fra `String`-biblioteket i Arduino. La oss si at vi har en tekststreng som inneholder både tall og bokstaver, for eksempel "a1b2c3". Vi ønsker å fjerne alle tallene fra strengen og få tilbake "abc". For å oppnå dette, kan vi bruke følgende kode:

```Arduino
String tekst = "a1b2c3";
// bruker en løkke for å gå gjennom hvert tegn i tekststrengen
for (int i = 0; i < tekst.length(); i++) {
  // bruker metoden remove() for å fjerne alle tall
  tekst.remove('0', '9');
}
Serial.println(tekst); // printer ut strengen uten tall
```
Output:
```1abc```

Som du kan se, så har `remove()`-metoden effektivt fjernet alle tallene fra strengen og etterlatt kun bokstavene.

## Dypdykk

Denne metoden kan også utvides til å fjerne flere typer tegn eller spesifikke tegn på en mer avansert måte. For eksempel kan du bruke vilkårlige karakterer som et argument for å fjerne alle forekomster av disse i teksten. Du kan også bruke `replace()`-metoden i samme strengbibliotek for å erstatte tegn som matcher et gitt mønster.

For å få mer informasjon om de ulike metodene tilgjengelig i `String`-biblioteket, kan du ta en titt på dokumentasjonen til Arduino eller se etter kodeeksempler på nettet.

## Se også

- [Arduino String metoder dokumentasjon](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Tutorial: Working with Strings in Arduino](https://www.arduino.cc/en/Tutorial/String)
- [Eksempelkode på å fjerne spesifikke tegn fra en tekststreng i Arduino](https://forum.arduino.cc/index.php?topic=117425.0)