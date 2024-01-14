---
title:                "Arduino: Uttrekking av delstrenger"
simple_title:         "Uttrekking av delstrenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Visual programming har nådd utrolige høyder de siste årene, og tusenvis av mennesker har begynt å lære å kode gjennom populære plattformer som Arduino. En av de viktigste funksjonene i Arduino er evnen til å trekke ut substrings fra en tekststreng. Dette kan være svært nyttig i ulike prosjekter, for eksempel å behandle sensordata eller lese og tolke informasjon fra en annen enhet. I denne bloggposten vil jeg vise deg hvorfor og hvordan du kan ekstrahere substrings i dine Arduino-prosjekter.

## Hvordan du gjør det

Å trekke ut substrings med Arduino er en relativt enkel oppgave, men det kan være forvirrende for nybegynnere. La oss se på et eksempel: Vi har en tekststreng som inneholder navn og alder på en person, og vi ønsker å trekke ut bare alderen. Følgende kode viser hvordan dette kan gjøres:

```Arduino
String person = "John 25";    // definere tekststreng
int index = person.indexOf(' ');    // finne plassering av mellomrom
String alder = person.substring(index+1);    // ekstrahere alderen fra plasseringen på mellomrommet
Serial.println(alder);    // skriv ut alderen
```

I denne koden bruker vi funksjonene `indexOf` og `substring` til å finne og ekstrahere substrings. Først finner vi plasseringen av mellomrommet ved å bruke `indexOf`-funksjonen. Deretter bruker vi `substring`-funksjonen til å ekstrahere alt etter dette mellomrommet, som i dette tilfellet er alderen. Til slutt skriver vi ut alderen ved hjelp av `Serial.println`-funksjonen. Kjører du denne koden, vil du få output "25" på seriellmonitoren.

## Dykk dypere

Hvis du ønsker å dykke dypere inn i utdrag av tekst, kan du også bruke `substring`-funksjonen til å definere en start og sluttindeks for utdraget. Dette kan være nyttig hvis det er mer enn ett mellomrom i tekststrengen, eller hvis du ønsker å trekke ut en spesifikk del av teksten. Hvis vi tar samme eksempel som tidligere, kan vi endre koden slik:

```Arduino
String person = "John 25 Smith";    // definere tekststreng
int index1 = person.indexOf(' ');    // finne plassering av første mellomrom
int index2 = person.lastIndexOf(' ');    // finne plassering av siste mellomrom
String etternavn = person.substring(index2+1);    // ekstrahere etternavnet fra plasseringen på siste mellomrom
String fornavn = person.substring(index1+1, index2);    // ekstrahere fornavnet fra plasseringen på første mellomrom til siste mellomrom
Serial.println(fornavn);    // skriv ut fornavnet
Serial.println(etternavn);    // skriv ut etternavnet
```

I denne koden bruker vi både `indexOf` og `lastIndexOf` for å finne plasseringen av de to mellomrommene i tekststrengen. Deretter bruker vi `substring`-funksjonen igjen, men denne gangen med både en start og sluttindeks som parameter. Kjører du denne koden, vil du få output "John" og "Smith" på seriellmonitoren.

## Se også

- `indexOf()` dokumentasjon: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/
- `substring()` dokumentasjon: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/