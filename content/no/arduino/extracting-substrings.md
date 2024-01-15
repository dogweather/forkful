---
title:                "Uttrekking av delstrenger"
html_title:           "Arduino: Uttrekking av delstrenger"
simple_title:         "Uttrekking av delstrenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å hente ut deler av en tekststreng, for eksempel bare de første fem bokstavene eller de siste tre tallene? Da vil du ha nytte av å lære å utvinne substrings i Arduino-programmering. Det kan hjelpe deg med å organisere og behandle data på en mer effektiv måte.

## Slik gjør du det

For å utvinne substrings i Arduino, kan du bruke funksjonene `substring()` eller `subString()`, avhengig av hvilken versjon av Arduino du bruker. Her er et eksempel på hvordan du kan bruke `substring()`-funksjonen for å hente ut de første fem bokstavene fra en tekststreng:

```Arduino
String ord = "Dette er en test";
String substring = ord.substring(0,5);

Serial.println(substring); // vil skrive ut "Dette" i Serial Monitor
```

I eksempelet ovenfor oppretter vi først en variabel `ord` som inneholder en tekststreng. Deretter bruker vi `substring()`-funksjonen på `ord`-variabelen, og angir at vi ønsker å hente ut bokstavene fra og med indeks 0 (det første tegnet i strengen) til og med indeks 5 (den sjette bokstaven i strengen). Den utvinne substringen blir lagret i variabelen `substring`, og vi kan deretter skrive ut den i Serial Monitor.

Hvis du bruker en eldre versjon av Arduino og må bruke funksjonen `subString()` i stedet, vil kodeeksempelet se slik ut:

```Arduino
String ord = "Dette er en test";
String substring = ord.subString(0,5);

Serial.println(substring); // vil skrive ut "Dette" i Serial Monitor
```

## Dykk dypere

Hvis du ønsker å hente ut deler av en tekststreng basert på bestemte kriterier, for eksempel bare tall eller bare store bokstaver, kan du kombinere substrings med andre funksjoner og metoder i Arduino. Dette kan være nyttig når du ønsker å organisere og bearbeide data på en mer presis måte.

Du kan også bruke substrings til å hente ut deler av en tekststreng som blir sendt inn via Serial Monitor, for eksempel ved bruk av kommandoen `Serial.readString()`.

## Se også

- Offisiell dokumentasjon for `substring()`-funksjonen i Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringfunctions/substring/
- Offisiell dokumentasjon for `subString()`-funksjonen i eldre versjoner av Arduino: https://www.arduino.cc/en/Reference/SubString