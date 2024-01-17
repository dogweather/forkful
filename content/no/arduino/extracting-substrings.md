---
title:                "Uthenting av delstrenger"
html_title:           "Arduino: Uthenting av delstrenger"
simple_title:         "Uthenting av delstrenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Ved å ekstrahere substrings i programmering, betyr det at vi splitter en streng av tekst i mindre deler. Dette kan være nyttig for å få tilgang til spesifikke deler av teksten eller behandle den på en annen måte. Programmerere bruker dette for å manipulere data og få tilgang til informasjon på en effektiv måte.

# Hvordan:

For å ekstrahere en substring i Arduino, bruker vi funksjonen ```substring```. Denne funksjonen tar to argumenter - startindeks og lengde. La oss se på et eksempel:

```
String tekst = "Hei alle sammen!";
String del = tekst.substring(4, 7);
Serial.println(del);
```
Resultatet av dette vil være "alle". Her starter vi å telle fra indeks 0, så 4 tilsvarer H, 5 tilsvarer e og 6 tilsvarer i. Siden vi har tilsiktet en lengde på 7, vil 7 tilsvarer mellomrommet etter i. Derfor vil vi bare få "alle" som resultat.

# Dypdykk:

Ekstrahering av substrings har vært en viktig del av programmering helt siden de første datamaskinene ble laget. Det har blitt brukt i mange forskjellige språk og har vist seg å være en effektiv måte å håndtere tekst på.

En annen måte å ekstrahere substrings på i Arduino er ved å bruke ```charAt```-funksjonen. Denne funksjonen tar en indeks som argument og returnerer tegnet på den indeksen i strengen. Her er et eksempel på hvordan du kan bruke det:

```
String tekst = "Hei alle sammen!";
char bokstav = tekst.charAt(6);
Serial.println(bokstav);
```
Dette vil gi oss bokstaven "s" som resultat.

# Se også:

Hvis du vil lære mer om strings i Arduino, kan du sjekke ut denne artikkelen: https://www.arduino.cc/reference/en/language/variables/data-types/string/.

Du kan også se denne videoen for å lære mer om hvordan du bruker ```substring```-funksjonen:  https://www.youtube.com/watch?v=kTZlBvFJ88I&t=317s.