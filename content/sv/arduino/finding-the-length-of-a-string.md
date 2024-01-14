---
title:    "Arduino: Hitta längden på en sträng"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna bestämma längden på en sträng är en viktig grundläggande kunskap inom Arduino-programmering. Det möjliggör för oss att hantera och manipulera text på ett effektivt sätt, vilket är särskilt användbart i projekt som involverar kommunikation med externa enheter eller användarinteraktion.

## Så här gör du

Att hitta längden på en sträng i Arduino är en enkel process som kan utföras med hjälp av en inbyggd funktion, `strlen ()`. Denna funktion beräknar och returnerar längden på en given sträng.

```Arduino
char str[] = "Hej världen!";
int length = strlen(str);
Serial.println(length); // Output: 13
```

Som ni kan se i exemplet ovan, deklarerar vi först en variabel `str` som innehåller vår sträng. Sedan använder vi `strlen ()` för att beräkna längden på strängen och tilldelar det till variabeln `length`. Till slut skriver vi ut längden på strängen till seriell monitor.

## Djupdykning

`strlen ()` är en standard C-funktion som finns tillgänglig i Arduino-miljön. Detta innebär att den inte bara kan användas för att hitta längden på en statisk sträng, som vi visade ovan, utan också för en dynamisk sträng som har tilldelats med `malloc ()` eller `new`.

En annan viktig aspekt att notera är att `strlen ()` räknar endast de faktiska tecken som finns i strängen, och inte inkluderar avslutande nollbyte. Detta är viktigt att ha i åtanke när man arbetar med textsträngar i olika manipulationer.

## Se även

- [Arduino Official Reference - strlen()](https://www.arduino.cc/reference/en/language/functions/string/functions/strlen/)
- [GeeksforGeeks - C String Functions](https://www.geeksforgeeks.org/c-string-functions-strlen-strcpy-strrev-/)