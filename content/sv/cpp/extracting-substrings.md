---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Substrängar i C++: Hur och Varför Man Extraherar 

## Vad & Varför? 
Extrahering av substrängar innebär att avskilja en liten del av en större sträng. Programmerare gör detta för att manipulera, söka i eller jämföra specifika delar av strängar.

## Hur man gör:
Funktionen `substr()` i C++ används för att extrahera substrängar. Låt oss ta en titt på ett exempel:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hej programmerare!";

    std::string substr = str.substr(4, 14);

    std::cout << substr;  // Skriver ut "programmerare!"
    
    return 0;
}
```
Ovanstående kod extraherar substrängen "programmerare!". Funktionen `substr()` tar två argument: startpositionen och antalet tecken att extrahera.
  
## Fördjupning 
Extrahering av substrängar i programmering har en lång historia. Det används frekvent för att lösa ett flertal problem, från textredigering till dataanalys.

I tidigare versioner av C++, innan `std::string` introducerades, använde man C-stilsträngar och teckenpekare för att arbeta med strängar. Men det ökade komplexiteten och skapade minneshanteringsproblem.

Det finns även alternativa sätt att extrahera substrängar i C++, särskilt med regex (reguljära uttryck) vilket ger dig mer möjlighet att anpassa din sökning.

Djupare in i kodningen, `substr()` funktionen skapar och returnerar en ny sträng. Det kan ha en prestandapåverkan om du arbetar med mycket stora strängar eller om din kod kör `substr()` ofta.

## Se också
För att ytterligare förstå konceptet extrahera substrängar rekommenderas följande källor:
- CPlusPlus.com: [std::string::substr](http://www.cplusplus.com/reference/string/string/substr/)
- Stack Overflow: [Understanding the use of 'std::string::substr'](https://stackoverflow.com/questions/4643512/when-we-use-substr-in-c)