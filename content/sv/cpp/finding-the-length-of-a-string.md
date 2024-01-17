---
title:                "Att hitta längden på en sträng"
html_title:           "C++: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng i C++ är ett vanligt problem som uppstår när man arbetar med text och strängar i sina program. Detta innebär att man behöver ta reda på hur många tecken som finns i en sträng för att kunna hantera den på rätt sätt. Detta är särskilt användbart när man behöver utföra operationer som att loopa igenom en sträng eller att jämföra två strängar.

## Hur man gör:
För att hitta längden på en sträng i C++, kan man använda funktionen `strlen()` som ingår i standardbiblioteket `cstring`. Här nedan finns ett exempel på hur man kan använda denna funktion:

```C++
#include <cstring>
#include <iostream>

int main() {
    char str[] = "Hej, världen!";
    int length = strlen(str);
    std::cout << "Längden på strängen är: " << length << std::endl;
    return 0;
}
```

Detta kommer att skriva ut "Längden på strängen är: 13" då det är just 13 tecken i strängen "Hej, världen!".

## Djupdykning:
För att förstå hur `strlen()` funktionen fungerar, är det viktigt att känna till lite bakgrundsinformation. I C++ behöver man inte ange längden på en sträng vid deklaration, utan en null-tecken (ASCII värde 0) läggs automatiskt till i slutet av strängen. `strlen()` funktionen räknar alltså helt enkelt antalet tecken i strängen tills den stöter på null-tecknet.

Om man inte vill använda sig av `strlen()`, finns det alternativa sätt att hitta längden på en sträng. Man kan till exempel använda sig av en loop som räknar antalet tecken manuellt, men detta kan kräva mer kod och ta längre tid.

## Se även:
- [C++ referens för strlen()](https://www.cplusplus.com/reference/cstring/strlen/) för mer detaljerad information om funktionen.
- [Null-tecken](https://www.ascii-code.com/) för att lära dig mer om hur det fungerar.