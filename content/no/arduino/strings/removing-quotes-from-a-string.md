---
date: 2024-01-26 03:36:54.208592-07:00
description: "Hvordan: For \xE5 fjerne anf\xF8rselstegn fra en streng i Arduino, kan\
  \ du l\xF8kke over tegnene og bygge opp strengen p\xE5 nytt uten anf\xF8rselstegnene.\
  \ For eksempel."
lastmod: '2024-03-13T22:44:41.046836-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 fjerne anf\xF8rselstegn fra en streng i Arduino, kan du l\xF8kke\
  \ over tegnene og bygge opp strengen p\xE5 nytt uten anf\xF8rselstegnene."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
For å fjerne anførselstegn fra en streng i Arduino, kan du løkke over tegnene og bygge opp strengen på nytt uten anførselstegnene. For eksempel:

```arduino
String removeQuotes(String str) {
  String resultat = ""; // Lager en tom streng for å holde på resultatet
  for (int i = 0; i < str.length(); i++) {
    if (str[i] != '"' && str[i] != '\'') { // Sjekk hvert tegn
      resultat += str[i]; // Legg til i resultatet hvis det ikke er et anførselstegn
    }
  }
  return resultat;
}

void setup() {
  Serial.begin(9600);
  String testStr = "'Hei, Verden!'";
  Serial.println(removeQuotes(testStr)); // Skal skrive ut: Hei, Verden!
}

void loop() {
  // Ingenting å gjøre her
}
```

Eksempelutskrift på Serial Monitor ville være:
```
Hei, Verden!
```

## Dypdykk
Konseptet med å fjerne tegn fra en streng er ikke unikt for Arduino; det er vanlig i mange programmeringsmiljøer. Historisk sett har funksjoner for strengmanipulering vært en kjernekomponent i programmeringsspråk for å tillate utviklere å rense og parse data effektivt.

I tillegg til manuelt å løkke og bygge en ny streng som vist ovenfor, finnes det alternative metoder. For eksempel kunne man bruke `replace()`-metoden til å erstatte anførselstegn med en tom streng, selv om det er kompromisser med tanke på lesbarhet og håndtering av escape-tegn.

```arduino
String removeQuotes(String str) {
  str.replace("\"", ""); // Erstatter alle doble anførselstegn
  str.replace("\'", ""); // Erstatter alle enkle anførselstegn
  return str;
}
```

Å forstå kompromissene er vitalt. Løkke-metoden kan være tregere for lange strenger, men er eksplisitt og enkel å tilpasse (som hvis du trengte å fjerne bare ledende og avsluttende anførselstegn). `Replace()`-metoden er mer kortfattet og generelt raskere, men det blir vanskeligere hvis det er behov for å håndtere escapede anførselstegn inne i strengen.

## Se også
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- W3Schools' guide til C++ strengmanipulering (relatert til Arduinos språk): https://www.w3schools.com/cpp/cpp_strings.asp
- Stack Overflow-diskusjoner om strengmanipulering i C++ (Arduinos basisspråk): https://stackoverflow.com/questions/tagged/string+cpp
