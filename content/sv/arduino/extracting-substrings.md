---
title:    "Arduino: Extrahera substrängar"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför

Att extrahera substrings kan vara användbart när man behöver behandla och manipulera textsträngar. Det kan vara till nytta i olika Arduino-projekt, till exempel för att identifiera specifika data eller för att skapa mer interaktiva program.

## Hur man gör

Det första steget i att extrahera substrings är att förstå strukturen i en textsträng. En textsträng är en samling av tecken som vanligtvis representerar ord, fraser eller meningar. För att extrahera en del av en textsträng, behöver vi veta dess position och längd.

För att hitta positionen för en viss del av texten kan vi använda funktionen `indexOf()`. Den här funktionen söker igenom en textsträng och returnerar positionen för det första förekommande tecknet. Om tecknet inte kan hittas, returneras -1. Om vi till exempel vill hitta positionen för bokstaven "l" i ordet "Arduino", skulle vi använda koden nedan:

```Arduino
String str = "Arduino";
int pos = str.indexOf("l");
```

För att ange längden på den substring vi vill extrahera kan vi använda funktionen `substring()`. Den här funktionen tar två argument - startpositionen och längden - och returnerar den del av textsträngen som börjar på den angivna positionen och har den angivna längden. Om vi vill extrahera ordet "duino" från textsträngen "Arduino" skulle vår kod se ut så här:

```Arduino
String str = "Arduino";
String substr = str.substring(2, 5);
```

Vi kan också kombinera dessa två funktioner för att hitta och extrahera en delsträng från en textsträng. Till exempel, om vi vill extrahera de tre sista bokstäverna från ordet "Arduino" skulle vår kod se ut så här:

```Arduino
String str = "Arduino";
String substr = str.substring(str.indexOf("n"), 3);
```

Slutligen, om vi bara vill extrahera en del av textsträngen utan att behöva ange en specifik längd, kan vi använda funktionen `substring()` med bara en startposition. Detta kommer att returnera resten av textsträngen från och med den angivna positionen. Om vi till exempel vill extrahera "rduino" från ordet "Arduino" skulle vår kod se ut så här:

```Arduino
String str = "Arduino";
String substr = str.substring(1);
```

## Djupdykning

Att extrahera substrings är ett kraftfullt verktyg för att manipulera textsträngar. Det finns många olika sätt att använda det på i Arduino-programmering, till exempel för att kontrollera inmatad data eller för att skapa dynamiska och interaktiva program.

Det är också viktigt att vara medveten om att funktionerna `indexOf()` och `substring()` bara fungerar med textsträngar som är lagrade i typen `String`. Om du använder C-strängar (som är en serie av tecken istället för en datatyp), måste du använda andra funktioner för att extrahera substrings.

Det finns också andra funktioner som kan vara användbara när man arbetar med textsträngar, som till exempel `charAt()`, som returnerar ett specifikt tecken från en given position, och `replace()`, som byter ut en del av en textsträng med en annan text.

## Se även

Här är några andra användbara resurser för att lära dig mer om att extrahera substrings i Arduino-programmering:

- [Referens för Arduino String-objektet](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino substring() referens](https://www.arduino.cc/reference/en/language/functions/string/substring/)
- [Arduino indexOf() referens](https://www.arduino.cc/reference/en/language/functions/string/indexof/)