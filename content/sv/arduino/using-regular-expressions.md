---
title:    "Arduino: Användning av reguljära uttryck"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför

I många fall när man programmerar med Arduino så behövs det att söka efter specifika mönster i en text eller sträng. Det kan vara allt från att leta efter specifika bokstäver eller siffror, till att ta bort eller byta ut delar av en sträng. Med hjälp av så kallade reguljära uttryck (regular expressions) kan du enkelt hantera detta och öppnar upp för mer kraftfulla möjligheter för din Arduino-kod.

## Så här gör du

För att använda reguljära uttryck i din Arduino-kod så behöver du först inkludera biblioteket "Regex" genom att skriva följande kod i början av ditt program:

```Arduino
#include <Regex.h>
```

Nästa steg är att skapa ett nytt regex-objekt och definiera det mönster du vill matcha. Du kan göra detta på följande sätt:

```Arduino
Regex myRegex("mönster");
```

Mönstret är det du vill matcha och kan vara en kombination av bokstäver, siffror och specialtecken. Sedan kan du använda funktionen "match()" för att söka igenom en sträng och få tillbaka en värdet som visar om det finns en matchning eller inte. Detta illustreras nedan:

```Arduino
if (myRegex.match("sträng att söka igenom")) {
  // Hantera matchning här
} else {
  // Ingen matchning hittad
}
```

För att få ut mer information om matchningen, som till exempel var i strängen matchningen hittades, kan du använda funktionen "getMatch()" som returnerar ett "Match_info" objekt. Till exempel:

```Arduino
Match_info matchInfo = myRegex.getMatch();
```

Match_info objektet innehåller sedan olika metoder som du kan använda för att hantera matchningen, till exempel "start()" och "end()" som returnerar start- respektive slutpositionen för matchningen i strängen.  

## Djupdykning

Reguljära uttryck är mycket kraftfulla verktyg för att hantera textsträngar och det finns många möjligheter för att skapa olika mönster och kombinationer av tecken för att matcha. Det finns också mer avancerade funktioner inom reguljära uttryck, som att byta ut eller ta bort delar av en sträng, som du kan utforska för att optimera din kod. Det finns många olika tutorials och guider tillgängliga online för att hjälpa dig komma igång med reguljära uttryck i Arduino.

## Se även

- [Officiell dokumentation för Regex biblioteket](https://www.arduino.cc/reference/en/libraries/regex/)
- [Grundläggande guide till reguljära uttryck i Arduino](https://create.arduino.cc/projecthub/SAnwandter1/regular-expressions-regex-for-arduino-b13ef3)
- [Mer avancerad guide för att använda reguljära uttryck i Arduino](https://blog.hackster.io/text-manipulation-in-arduino-using-regular-expressions-with-regex-a9e7cd49a1bd)