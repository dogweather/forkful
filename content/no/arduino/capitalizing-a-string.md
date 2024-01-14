---
title:                "Arduino: Store bokstaver i en streng"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle du bry deg om å skrive en kode som gjør at bokstaver blir store? Vel, det kan være mange grunner til dette. Kanskje du ønsker at teksten på en LCD-skjerm skal være i store bokstaver for å gjøre den mer lesbar. Eller kanskje du ønsker at en bestemt del av koden skal skrive ut en melding i store bokstaver for å tiltrekke seg oppmerksomheten din. Uansett hva grunnen er, kan det være nyttig å vite hvordan du kan få bokstaver til å bli store i Arduino-programmering.

## Hvordan

Programmering av en funksjon for å få bokstaver til å bli store i Arduino er faktisk ganske enkelt. Du trenger bare å følge disse trinnene:

1. Start med å opprette en variabel som inneholder teksten du ønsker å kapitalisere, for eksempel `tekst`.
2. Lag en `for`-løkke som går gjennom alle bokstavene i variabelen `tekst` ved hjelp av funksjonen `length()`.
3. Bruk funksjonene `tolower()` og `toupper()` for å konvertere små bokstaver til store bokstaver.
4. Opprett en ny variabel, for eksempel `kapitalisertTekst`, som inneholder den kapitaliserte teksten.
5. Skriv ut den nye variabelen ved hjelp av `Serial.println()`-funksjonen.

```Arduino
String tekst = "dette er en tekst";
String kapitalisertTekst;

for (int i = 0; i < tekst.length(); i++) {
  kapitalisertTekst += toupper(tekst[i]);
}

Serial.println(kapitalisertTekst);
```

Output: `DETTE ER EN TEKST`

Det er også viktig å merke seg at `toupper()`-funksjonen bare vil fungere for engelsk alfabet. Hvis du trenger å kapitalisere teksten for andre språk, må du bruke en annen funksjon som støtter dette språket.

## Dypdykk

Å kapitalisere teksten kan virke som en enkel oppgave, men det er viktig å forstå hvordan den faktisk fungerer i koden. `for`-løkken går gjennom hver bokstav i teksten og bruker `toupper()`-funksjonen til å konvertere den til store bokstaver. Det er også mulig å bruke andre funksjoner, som for eksempel `toascii()`, for å konvertere bokstaver til store bokstaver. Det er viktig å eksperimentere med ulike funksjoner for å se hva som fungerer best for ditt spesifikke behov.

## Se også

- [Arduino Tutorial: Strings](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringConstructors/)
- [ASCII Table](https://www.asciitable.com/)