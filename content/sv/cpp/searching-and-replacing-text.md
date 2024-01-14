---
title:                "C++: Sökning och ersättning av text"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en vanlig procedur inom programmering, oavsett om det handlar om att ändra en stavning eller att uppdatera variablnamn. Genom att lära sig hur man effektivt söker och ersätter text i C++ kan man spara tid och undvika manuella misstag.

## Hur man gör
För att söka och ersätta text i C++, behöver man använda sig av två huvudfunktioner: `find()` och `replace()`. `find()` kommer att leta igenom en sträng efter en viss text och returnera positionen där texten hittades. `replace()` kommer sedan att byta ut den hittade texten med en annan.

Här är ett kodexempel som visar hur man använder `find()` och `replace()` för att byta ut alla förekomster av ordet "hej" till "hello":

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hej värld!";
    std::string hello = "hello";
    
    // Letar efter första förekomsten av "hej"
    size_t found = str.find("hej");
    
    if (found != std::string::npos) { // Om "hej" hittas
        // Byt ut "hej" med "hello"
        str.replace(found, 3, hello);
    }
    
    std::cout << str; // Skriver ut "Hello värld!"
    
    return 0;
}
```

Output:
```
Hello värld!
```

## Djupdykning
När det kommer till sökning och ersättning av text i C++, finns det vissa saker att tänka på för att undvika vanliga misstag. Till exempel, `find()` returnerar positionen där texten hittades som en `size_t`variabel, vilket är ett unsigned integer. Detta betyder att om "hej" inte hittas i strängen, kommer `find()` att returnera det största möjliga värdet för en `size_t`, vilket kan leda till oväntade resultat. 

En annan viktig aspekt att tänka på är att `replace()` inte ersätter text i själva strängen, utan genererar en ny sträng med de ändringar som gjorts. För att ändra originalsträngen behöver man tilldela resultatet av `replace()` tillbaka till den.

## Se även
- [C++ Dokumentation för `find()`](https://www.cplusplus.com/reference/string/string/find/)
- [C++ Dokumentation för `replace()`](https://www.cplusplus.com/reference/string/string/replace/)
- [En guide för grundläggande C++ strängmanipulering](https://www.learncpp.com/cpp-tutorial/6-x-chapter-6-comprehensive-data-decision-and-looping-statements/6-10-string-manipulation/)

Se även
 - [Markdown: Syntax](https://daringfireball.net/projects/markdown/syntax)
 - [Get Started with Markdown in Visual Studio Code](https://code.visualstudio.com/docs/languages/markdown)