---
title:    "Arduino: Sletting av tegn som matcher et mønster"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å fjerne bestemte tegn fra en tekststreng i Arduino-programmering. Dette kan være for å rydde opp i unødvendig informasjon eller for å tilpasse strengen til et spesifikt format. Uansett årsak, er det nyttig å vite hvordan man kan slette tegn som matcher et bestemt mønster i en tekststreng. Les videre for å lære mer!

## Hvordan gjøre det

Å slette tegn som matcher et mønster i en tekststreng kan gjøres ved hjelp av funksjonen `removeIf()`. Denne funksjonen tar inn to parametere: et tegn (eller et sett med tegn) som skal slettes og tekststrengen som skal endres.

```Arduino
String tekststreng = "Hei! Du kan slette alle vokalene fra denne setningen.";
tekststreng.removeIf("aeiou");
```

Koden over vil slette alle vokalene fra tekststrengen, og resultatet blir "H! D kn sll tll vkl fr dnn stnnng."

## Dypdykk

For å forstå hvordan `removeIf()` fungerer, må vi se på hvordan den er implementert. Funksjonen itererer gjennom hvert tegn i tekststrengen og sammenligner det med hvert tegn i det andre parameteret. Hvis tegnet matcher et tegn i det andre parameteret, blir det fjernet fra tekststrengen. Dette skjer for hvert tegn i tekststrengen.

En ting å merke seg er at `removeIf()` fungerer forskjellig avhengig av hvilken versjon av Arduino du bruker. I nyere versjoner fungerer den som beskrevet ovenfor, men i eldre versjoner vil den bare fjerne det første forekomsten av tegnet og stoppe. Det kan også være begrensninger på hvor mange tegn som kan fjernes i en tekststreng, som er viktig å være klar over når du bruker denne funksjonen.

## Se også

- [Mer informasjon om `removeIf()` funksjonen](https://www.arduino.cc/reference/en/language/variables/string/functions/removeif/)
- [Eksempelkode for å fjerne tegn fra en tekststreng](https://www.arduino.cc/en/Tutorial/StringReplace)
- [Mer informasjon om tekstbehandling i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)