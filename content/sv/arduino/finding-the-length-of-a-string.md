---
title:                "Arduino: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande färdighet inom programmering, och är särskilt användbar inom Arduino-programmering. Det låter dig hantera och manipulera textdata på ett effektivt sätt.

## Så här gör du

För att hitta längden på en sträng i Arduino, använder vi den inbyggda funktionen `strlen()`. Detta gör att vi kan räkna antalet tecken i en given sträng och returnera detta värde. Här är ett exempel på hur du kan använda `strlen()` i ditt program:

```Arduino
char mittNamn[] = "Sara";
int langd = strlen(mittNamn); // Längden av "Sara" är 4

Serial.println(langd); // Skriver ut 4 i serieporten
```

I det här exemplet har vi skapat en variabel `mittNamn` och gett den en sträng, "Sara". Sedan använder vi `strlen()` för att räkna längden på strängen och tilldelar det värdet till en annan variabel, `langd`. Slutligen skriver vi ut längden på strängen i serieporten.

Det är viktigt att komma ihåg att `strlen()` endast fungerar på noll-terminerade strängar, vilket innebär att strängen måste avslutas med ett speciellt tecken, `'\0'`. Om strängen inte har detta tecken kommer `strlen()` att räkna fel längd på strängen.

## Djupdykning

När du använder `strlen()` finns det några viktiga saker att tänka på för att undvika buggar i ditt program. En av de vanligaste är att försäkra sig om att strängen är tillräckligt stor för den data som ska lagras i den. Om strängen är för liten kan det leda till en så kallad "buffer overflow", som kan orsaka allvarliga problem i ditt program.

En annan viktig sak att tänka på är att `strlen()` returnerar ett `int`-värde, vilket innebär att den maximala längden på en sträng är 2 147 483 647 tecken. Om du försöker beräkna längden på en större sträng kan det leda till fel i ditt program.

## Se även

* [Arduino Reference - strlen()](https://www.arduino.cc/reference/en/language/variables/data-types/strlen/)
* [C++ Reference - strlen()](https://www.cplusplus.com/reference/cstring/strlen/)