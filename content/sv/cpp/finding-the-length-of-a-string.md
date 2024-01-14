---
title:    "C++: Att hitta längden på en sträng"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift vid programmering och kan vara till hjälp vid olika situationer, såsom formatering av utskrifter eller jämförelser av textsträngar. Att lära sig hur man gör detta kan utöka din kunskap och förbättra din kodningserfarenhet.

## Hur man gör det

För att hitta längden på en sträng i C++ finns det en inbyggd funktion som heter `length()` som returnerar en `int` som motsvarar strängens längd. Här är ett exempel på hur du kan använda den:

```C++
#include <iostream>
#include <string>

int main() {
  // Deklarerar en sträng
  std::string myString = "Hej där!";

  // Använder length() funktionen för att hitta längden på strängen
  int length = myString.length();

  // Skriver ut längden på strängen 
  std::cout << "Längden på strängen är: " << length << std::endl;

  return 0;
}
```

#### Output
```
Längden på strängen är: 8
```

## Djupdykning

Det finns två olika sätt att hitta längden på en sträng i C++. Utöver `length()` funktionen som vi redan har sett, finns det också en `size()` funktion som i grunden gör samma sak. Skillnaden är att `length()` är en medlem av `string` klassen medan `size()` är en funktion som ärver från `template class basic_string`. Båda returnerar samma värde och använder samma algoritm för att hitta längden på strängen, så det spelar ingen roll vilken av dem du väljer att använda.

Det finns också andra sätt att hitta längden på en sträng, såsom att använda en `for loop` för att loopa igenom strängen och räkna antalet tecken. Men detta kan vara mer tidskrävande och mindre effektivt jämfört med att använda de inbyggda funktionerna.

## Se även

- [C++ String](https://www.geeksforgeeks.org/cpp-strings/) - för mer information om hantering av strängar i C++
- [Length vs Size in C++](https://www.programiz.com/cpp-programming/library-function/string/length-vs-size) - för en djupare förståelse av skillnaderna mellan `length()` och `size()` funktionerna