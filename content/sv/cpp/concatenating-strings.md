---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Strängkonkatenering i C++: En snabbguide
Här kommer vi att diskutera en viktig aspekt inom programmering: strängkonkatenering, eller sammanfogning av strängar, i C++. 

## Vad och Varför?
Strängkonkatenering är processen att sammankoppla två eller flera strängar till en enda sträng. Programmerare gör detta för att skapa dynamiska meddelanden, formatera utdata eller bygga SQL-frågor.

## Hur man gör:
Här är några exempel på hur man genomför strängkonkatenering i C++:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str1 = "Hej ";
    std::string str2 = "Sverige!";
    // Sammanfoga strängarna
    std::string str3 = str1 + str2;

    std::cout << str3;
    return 0;
}
```
*I output: *
```
Hej Sverige!
```

## Fördjupning
**Historisk kontext:** Före C++11 var konkatenering av strängar lite knepigt, vanligtvis gjort med funktioner som `strcat()` eller `sprintf()`. Men nu, med överbelastning av `operator+`, är det mycket enklare.

**Alternativ:** Utöver `operator+` kan du också använda `append()`, en inbyggd funktion i `std::string`-klassen. Det ger samma resultat men kan vara mer effektivt för stora strängar.

**Implementeringsdetaljer:** När du använder `operator+` för konkatenering skapas en ny sträng som innefattar de sammanfogade strängarna. Det är viktigt att notera för prestanda och minneshantering.

## Se även:
För mer detaljerad information, se dessa källor:
- [Concatenation (operators + and +=)](https://www.cplusplus.com/reference/string/string/operator+/)
- [Basic String Operations](http://www.cplusplus.com/doc/tutorial/ntcs/?kw=string)
- [W3Schools C++ Strings](https://www.w3schools.com/cpp/cpp_strings.asp)