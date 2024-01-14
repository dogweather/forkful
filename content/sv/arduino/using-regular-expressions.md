---
title:                "Arduino: Användning av regelbundna uttryck"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Reguljära uttryck är en kraftfull datamodellerings- och uttryckssyntax som används för att söka, matcha och manipulera textsträngar. Genom att använda reguljära uttryck i din Arduino-programmering, kan du effektivt hantera och bearbeta textbaserade indata på ett mer flexibelt och dynamiskt sätt.

## Så här gör du
För att använda reguljära uttryck i din Arduino-kod, behöver du inkludera ett bibliotek som heter "Regex" i början av ditt program. Sedan kan du använda funktioner som "regexMatch()" och "regexReplace()" för att söka och manipulera textsträngar enligt ett visst mönster. Nedan är ett exempel på hur du kan använda reguljära uttryck för att söka efter ett telefonnummer i en text och sedan ersätta det med ett annat nummer:

```Arduino
#include <Regex.h>

void setup() {
  Serial.begin(9600); // Initiera seriell kommunikation
  String text = "Detta är mitt telefonnummer: 123-456-7890"; //Teststräng med telefonnummer
  regexReplace(text, "[0-9]{3}-[0-9]{3}-[0-9]{4}", "555-555-5555"); // Sök och ersätt telefonnumret
  Serial.println(text); // Skriv ut den uppdaterade texten
}

void loop() {
  // Tom loop
}
```

Det föreslagna mönstret "[0-9]{3}-[0-9]{3}-[0-9]{4}" söker efter en textsträng som består av tre siffror, följt av ett bindestreck, följt av tre siffror, följt av ett till bindestreck, följt av fyra siffror. När den hittar ett matchande mönster i texten, kommer den att ersättas med "555-555-5555".

Andra vanliga funktioner som du kan använda med reguljära uttryck är "regexSearch()" för att söka efter ett mönster i en text och "regexMatchList()" för att returnera en lista med alla matchade mönster i texten.

## Djupdykning
För att bli mer bekant med reguljära uttryck och dess syntax, är det viktigt att fortsätta öva och experimentera med olika mönster och funktioner. Det finns också flera online-resurser och böcker tillgängliga för att hjälpa dig att lära dig mer om reguljära uttryck och hur du kan använda dem i din Arduino-programmering.

## Se också
- [Arduino Regex bibliotek](https://github.com/mikaelpatel/Arduino-regex)
- [Reguljära uttryck tutorial](https://www.regular-expressions.info/tutorial.html)
- [Söka och ersätta med reguljära uttryck på Arduino](https://forum.arduino.cc/index.php?topic=526077.0)