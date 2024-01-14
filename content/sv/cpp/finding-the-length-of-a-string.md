---
title:    "C++: Att hitta längden på en sträng."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en viktig del av programmering eftersom det tillåter oss att arbeta med textdata på ett effektivt sätt. Det finns många situationer där vi behöver veta hur många tecken som finns i en sträng, så att vi kan manipulera den på rätt sätt.

## Hur man gör det
För att hitta längden på en sträng i C ++, kan vi använda den inbyggda funktionen `length ()`. Vi behöver bara ange vår sträng som argument och sedan spara returvärdet i en variabel. Här är ett exempel på hur detta kan se ut:

```C++
#include <iostream>
#include <string> 

using namespace std; 

int main() 
{ 
    string namn = "Lisa"; 
    int langd = namn.length(); 
    cout << "Längden på namnet är: " << langd << " tecken" << endl; 
    return 0; 
}
```

Output: 
```
Längden på namnet är: 4 tecken
```

Som du kan se är `length ()` enkel att använda och ger oss direkt längden på vår sträng. Det är också viktigt att notera att funktionen inte tar med nolltoleransen (null-terminator) vid räkningen, vilket är en viktig egenskap att veta när du arbetar med strängar.

## Djupdykning
Det finns olika sätt att implementera funktionen `length ()` i C ++, men den vanligaste metoden är att använda en loop som går igenom tecken för tecken tills den når nolltoleransen. Detta är vad som händer bakom kulisserna när vi använder den inbyggda funktionen. Detta gör att vi kan få längden på alla typer av strängar, oavsett om de innehåller specialtecken eller inte.

En annan viktig aspekt att notera är att längden på en sträng i C ++ är olika från storleken på strängen. Längden är antalet tecken, medan storleken är antalet bytes som används för att lagra strängen. Detta beror på att vissa tecken kan ta upp fler än ett byte i minnet.

## Se även
* [C ++ Strings](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
* [Strängsökningsfunktioner i C ++](https://www.learncpp.com/cpp-tutorial/72-built-in-string-functions/)
* [C ++ Standard Template Library (STL)](https://www.learncpp.com/cpp-tutorial/standard-template-library-stl/)

Tack för att du läste! Hoppas du har lärt dig något nytt och är redo att använda `length ()` för att hitta längden på dina strängar i dina framtida C ++ projekt. Lycka till!