---
title:                "Arduino: Sammanslagning av strängar"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Att sammanfoga eller "concatenate" strängar är en vanlig operation inom programmering för att kombinera flera enskilda ord eller tecken till en enda sträng. Detta kan vara användbart för att skapa dynamiska meddelanden eller för att skapa variabler med olika värden.

## Hur man gör:
För att sammanslå strängar i Arduino krävs det att man använder en speciell funktion som kallas för "concat()". Denna funktion kan användas tillsammans med olika datatyper som int, float och char.

```Arduino
//Exempel på hur man sammanslår två strängar
String str1 = "Hej ";
String str2 = "Arduino";

String result = str1.concat(str2);

Serial.println(result); // Resultatet blir "Hej Arduino"
```

Det är även möjligt att sammanslå flera strängar samtidigt genom att använda funktionen multiple times:

```Arduino
//Exempel på hur man sammanslår flera strängar
String str1 = "Dessa ";
String str2 = "är ";
String str3 = "många ";
String str4 = "strängar";

String result = str1.concat(str2).concat(str3).concat(str4);

Serial.println(result); // Resultatet blir "Dessa är många strängar"
```

För att sammanslå en sträng med en numerisk variabel kan man använda sig av funktionen "concat(String string, int var)" där första argumentet är den sträng som man vill ha concatenaten på och det andra argumentet är den numeriska variabeln.

```Arduino
//Exempel på hur man sammanslår en sträng med en numerisk variabel
String str1 = "Min ålder är: ";
int age = 27;

String result = str1.concat(age);

Serial.println(result); // Resultatet blir "Min ålder är: 27"
```

## Djupdykning
Vid skapandet av strängar i Arduino är det viktigt att komma ihåg att det påverkar minnet på mikrokontrollern. Ju fler strängar som sammanslogs, desto mer minne kommer det att ta upp. Det är även viktigt att hålla koll på antalet tecken i de sammanslagna strängarna, eftersom Arduino har en begränsning på 64 tecken för varje sträng.

En annan viktig faktor är att hantera specialtecken som till exempel "åäö" inom Arduino. För att undvika problem med dessa tecken är det bäst att använda sig av Unicode-kompatibla teckenkodningar som "UTF-8".

## Se även
- [Concatenate Strings in Arduino - GeeksforGeeks](https://www.geeksforgeeks.org/concatenate-strings-in-arduino/)
- [Arduino Strings - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Concat() - Arduino Forum](https://forum.arduino.cc/index.php?topic=39642.0)