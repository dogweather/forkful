---
title:                "Arduino: Uttrekking av substringer"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med Arduino-programmering, er det ofte behov for å manipulere strenger med tekst. Dette kan innebære å hente ut et lite stykke av teksten, kjent som en substring. Dette kan være nyttig når du for eksempel ønsker å vise en del av en lengre melding, eller når du trenger å sammenligne to deler av en tekststreng.

## Hvordan

For å ekstrahere substrings i Arduino, bruker vi funksjonen `substring()`. Denne funksjonen tar inn to parametere: startindeks og slutindeks. Startindeksen angir hvor i strengen du ønsker å begynne å hente ut tekst, mens slutindeksen angir hvor du ønsker å avslutte.

```Arduino
String myString = "Hei, dette er en test";
String extractedString = myString.substring(5, 9);

Serial.println(extractedString);
```

Kjører dette koden vil gi følgende output: `dette`

Du kan også bruke `substring()` til å hente ut resten av strengen fra en gitt startindeks:

```Arduino
String myString = "Hei, dette er en test";
String extractedString = myString.substring(5);

Serial.println(extractedString);
```

Dette vil gi følgende output: `dette er en test`

Merk at startindeksen i dette tilfellet er inkludert i det ekstraherte substringet.

## Dypdykk

Det er noen viktige ting å være klar over når du bruker `substring()` i Arduino-programmering:

- Indeksene begynner alltid på 0. Dette betyr at første tegn i strengen har indeks 0.
- Slutindeksen er valgfri. Hvis den ikke angis, vil funksjonen hente ut resten av strengen fra startindeksen.
- Hvis du angir en slutindeks som er større enn lengden på strengen, vil funksjonen bare hente ut teksten frem til slutten av strengen.
- Du kan ikke bruke negative indekser, som kan være vanlig i andre programmeringsspråk.

## Se også

- [Arduino referanseguide om `substring()` (engelsk)](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [W3Schools om `substring()` (engelsk)](https://www.w3schools.com/jsref/jsref_substring.asp)
- [Tutorial om tekstbehandling i Arduino (engelsk)](https://sportysnetwork.com/teachsomebody/how-to-handle-text/?et=wzfscf55fcp)