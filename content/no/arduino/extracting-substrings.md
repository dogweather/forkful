---
title:    "Arduino: Utnytte substringer"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substrings kan være en nyttig teknikk når du jobber med Arduino-programmering. Dette lar deg hente ut deler av en tekststreng eller et tall, noe som kan være nyttig for å håndtere data eller variabler på en mer effektiv måte.

## Hvordan

For å utvinne en substring i Arduino, kan du bruke funksjonene `substring()` eller `subString()` avhengig av hvilken versjon av Arduino IDE du bruker. Disse funksjonene tar inn tre parametere: startindeks, sluttindeks og en tekststreng du vil utvinne substringen fra.

```Arduino
String tekst = "Hei verden!";
String del = tekst.substring(4,8); // henter ut "verd"
```

I dette eksempelet, starter substringen på indeks 4 og slutter på indeks 8 (indeksene starter alltid på 0).

## Dypdykk

Det kan være nyttig å forstå hvordan substringfunksjonene fungerer under overflaten. Når du bruker `substring()` eller `subString()`, oppretter Arduino egentlig en ny tekststreng og fyller den med tegnene fra den opprinnelige strengen som ligger mellom start- og sluttindeksen. Deretter returnerer funksjonen denne nye substringen.

Det er også verdt å merke seg at indeksene kan være både positive og negative tall, hvor det negative tallet starter fra slutten av strengen. For eksempel, hvis du vil hente ut den siste delen av en tekststreng som er lagret i `tekst`, kan du bruke `substring(9, tekst.length())` eller `subString(-7, tekst.length())`.

## Se også

- [Arduino String Class Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Substring documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [SubString documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring-2/)