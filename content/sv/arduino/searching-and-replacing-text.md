---
title:    "Arduino: Sökning och ersättning av text"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en viktig funktion i programmering för att effektivt kunna ändra textbaserad data. Det är också ett praktiskt sätt att korrigera felaktig eller föråldrad kod.

## Så här gör du
Det finns flera olika sätt att söka och ersätta text i Arduino-program. Ett av de enklaste och mest användbara sätten är att använda funktionen "replace" i Arduino String-biblioteket. Nedan följer ett exempel på en kod som söker och ersätter text i en variabel:
``` Arduino
String text = "Hej, mitt namn är Arduino!";
text.replace("Arduino", "Elsa");
Serial.println(text);

```
Detta kommer att skriva ut "Hej, mitt namn är Elsa!" på seriell monitor. Det är viktigt att notera att "replace" funktionen är fallkänslig, vilket innebär att den måste matcha exakt för att bytas ut.

## Fördjupning
Det finns flera andra sätt att söka och ersätta text i Arduino, som att använda reguljära uttryck eller att manuellt loopa igenom en sträng för att hitta och ersätta specifika tecken eller ord. Det är också viktigt att förstå skillnaderna mellan olika datatyper och deras olika funktioner för att på bästa sätt kunna använda sök- och ersättningstekniker.

## Se även
- [Arduino String-biblioteket](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Söka och ersätta med reguljära uttryck i Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/serialbegin/)
- [Skillnaden mellan datatyper i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/)