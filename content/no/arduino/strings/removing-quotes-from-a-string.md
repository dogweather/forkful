---
aliases:
- /no/arduino/removing-quotes-from-a-string/
date: 2024-01-26 03:36:54.208592-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng inneb\xE6rer \xE5 strippe\
  \ vekk eventuelle forekomster av enkle (`'`) eller doble (`\"`) anf\xF8rselstegn\
  \ som omslutter teksten.\u2026"
lastmod: 2024-02-18 23:08:54.129894
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng inneb\xE6rer \xE5 strippe vekk\
  \ eventuelle forekomster av enkle (`'`) eller doble (`\"`) anf\xF8rselstegn som\
  \ omslutter teksten.\u2026"
title: "Fjerne anf\xF8rselstegn fra en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng innebærer å strippe vekk eventuelle forekomster av enkle (`'`) eller doble (`"`) anførselstegn som omslutter teksten. Programmerere gjør ofte dette for å rense inndata, forberede strenger for sammenligning, eller behandle tekstdata som ved et uhell kan inkludere anførselstegn som en del av strenginnholdet.

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
