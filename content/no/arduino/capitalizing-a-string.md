---
title:    "Arduino: Å kapitalisere en streng"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med tekst i en Arduino, har du kanskje lurt på hvordan du kan gjøre tekst mer lesbar og estetisk tiltalende. En måte å gjøre dette på er å kapitalisere strenger, som betyr å gjøre første bokstav i hvert ord til en stor bokstav. Dette kan være nyttig for å presentere tekst på en mer profesjonell måte eller for å få en enhet til å skille seg ut.

## Hvordan gjøre det

Først må du velge hvilken streng du vil formatere. Deretter må du bruke en funksjon som heter "capitalize". Denne funksjonen er innebygd i Arduino-språket og gjør nøyaktig det navnet antyder - den kapitaliserer strenger. For å bruke denne funksjonen, må du først kalle den mens du bruker strengens navn og deretter skrive "capitalize()".

```Arduino
String tekst = "hallo verden";
String formatert = tekst.capitalize();
Serial.println(formatert);
```

Utskriften av dette vil være "Hallo Verden". Du kan gjenta denne prosessen for så mange strenger du ønsker, og hver enkelt vil bli formatert ved hjelp av denne enkle funksjonen.

## Dypdykk

Dette er et enkelt eksempel på hvordan man kan kapitalisere strenger i Arduino, men det er viktig å merke seg at denne funksjonen bare vil kapitalisere det første tegnet i hver streng. Hvis du vil kapitalisere andre bokstaver, må du bruke en mer komplisert metode som innebærer å splitte strengen opp i ord og endre hver enkelt bokstav.

Det er også verdt å nevne at denne funksjonen bare kapitaliserer engelske bokstaver, så hvis du vil bruke den på strenger som inneholder andre språk, må du først bruke en funksjon som setter språket i enheten din.

## Se også

- [Funksjonen "capitalize" i Arduino referansedokumentasjon](https://www.arduino.cc/en/Reference/StringCapitalize)
- [Hvordan splitte en streng i Arduino](https://randomnerdtutorials.com/arduino-string-functions-fixed-and-advanced/#split)