---
title:                "Sammanslagning av strängar"
html_title:           "Arduino: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Är du frustrerad över att behöva skriva ut flera separata teckensträngar när du vill visa dem tillsammans? Bra nyheter, detta är precis vad strängkonkatinering är till för! Detta är en användbar teknik som hjälper dig att effektivt sammanfoga flera strängar och presentera dem som en enda enhet.

## Hur man gör
För att sammanfoga strängar i Arduino, används operatorn "+" eller funktionen "strcat()". Här är ett enkelt exempel:
``` Arduino
String str1 = "Hello";
String str2 = "world!";
String str3 = str1 + " " + str2; // str3 blir "Hello world!"
```
Om du vill använda "strcat()", så kan ett exempel se ut så här:
``` Arduino
char str1[] = "This is ";
char str2[] = "a sentence.";
strcat(str1, str2); // str1 blir "This is a sentence."
```
Det är viktigt att känna till att strängkonkatinering endast fungerar om båda variablerna är av samma datatyp, till exempel "String" eller "char". Om du försöker använda "+" eller "strcat()" mellan två olika datatyper kommer du få ett felmeddelande.

## Djupdykning
För att förstå strängkonkatinering bättre är det viktigt att förstå hur datatyper fungerar i Arduino. En datatyp representerar typen av data som en variabel lagrar och det är därför viktigt att matcha datatyperna när man sammanställer strängar. Om datatyperna är olika, kommer Arduino försöka konvertera dem för att matcha och det kan leda till oönskade resultat. Det är därför som man bör använda samma datatyp för båda variablerna. En annan viktig sak att notera är att "String" data skapar en ny variabel med en ny plats i minnet varje gång en sträng är uppdaterad, vilket kan leda till minnesproblem. Om man konstant använder "String" i stället för "char" när man arbetar med strängar, kan det påverka kodens prestanda.

## Se även
Om du vill lära dig mer om datatyper och hur de påverkar din kod i Arduino, kolla in följande länkar:
- [Datatyper i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/)
- [Skillnaden mellan "String" och "char" i Arduino](https://arduino.stackexchange.com/questions/347/how-do-i-declare-a-string-in-arduino)
- [Strängoperationer i Arduino](https://www.programiz.com/arduino/strings)