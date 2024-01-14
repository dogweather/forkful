---
title:    "Arduino: Användning av reguljära uttryck"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Varför använda reguljära uttryck i Arduino programmering
Att använda reguljära uttryck i Arduino programmering kan göra kodningen mer effektiv och strukturerad. Det gör det möjligt att söka efter mönster i stället för konkreta strängar och underlättar därmed hanteringen av data.

## Så här använder du reguljära uttryck i Arduino
För att använda reguljära uttryck i Arduino måste du först inkludera biblioteket "Regexp", detta görs genom att lägga till följande kod i början av ditt program:
```Arduino
#include <Regexp.h>
```
För att söka efter ett mönster i en sträng använder du funktionen "regexMatch()", tillsammans med pattern och input variabler:
```Arduino
int pattern = "lidande";
int input = "går du igenom något lidande idag?";
bool match = regexMatch(pattern, input);
```
Om mönstret finns i strängen kommer "match" att ha värdet "true", annars kommer det att vara "false". Du kan också använda reguljära uttryck för att hitta matchningar och extrahera dem från en sträng. Till exempel:
```Arduino
int pattern = "(\\d+)-(\\d+)";
int input = "Min födelsedag är den 31-12";
String match = regexExtract(pattern, input);
```
Här kommer "match" att innehålla "31-12". Du kan också använda reguljära uttryck för att ersätta delar av en sträng:
```Arduino
int pattern = "himmelskt";
int replacement = "awesome";
int input = "Den här tårtan är himmelsk!";
String result = regexReplace(pattern, replacement, input);
```
"result" varar får strängen "Den här tårtan är awesome!". Det finns många andra funktioner för att använda reguljära uttryck i Arduino, så se till att kolla in dokumentationen för fler exempel.

## Utforska reguljära uttryck ännu mer
Om du vill lära dig mer om reguljära uttryck och hur du kan använda dem i Arduino rekommenderar vi att du tittar på dessa resurser:
- [Arduino Regexp biblioteket](https://www.arduino.cc/reference/en/libraries/regexp/)
- [Tutorial video om reguljära uttryck i Arduino](https://www.youtube.com/watch?v=saTxTKCPWuA)
- [Reguljära uttryck Cheat Sheet för Arduino](https://github.com/ahmadyassir/arduino-regex-cheat-sheet)

## Se också
- [Översättning av "regular expressions" i Svenska](https://lexikon.dinstudio.se/sok.php?ord=regular+expressions)