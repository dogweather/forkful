---
title:                "Extrahera delsträngar"
html_title:           "C++: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

Hej programmerare!

## Vad & Varför?

Att extrahera substrängar betyder att man tar ut en mindre del av en större sträng. Det kan vara användbart när man behöver komma åt en specifik del av en väldigt lång sträng. Programmerare använder detta verktyg för att effektivisera och förenkla sin kodning.

## Så här:

```C++
#include <iostream> 
#include <string> 

int main() { 
   std::string ord = "Programmering"; 
   std::cout << ord.substr(3,5) << std::endl; 
return 0; 
}
```

Output: "gramm"

```C++
#include <iostream> 
#include <string>
int main() { 
   std::string mening = "Jag älskar att koda!"; 
   std::cout << mening.substr(8,4) << std::endl; 
return 0; 
}
```

Output: "att k"

## Deep Dive:

Extrahera substrängar har funnits länge inom programmeringen och är ett viktigt verktyg för att hantera textdata. Istället för att behöva manipulera hela strängen kan man enkelt extrahera en del som behövs. Det finns även andra sätt att göra detta, som användning av arrayer och loopar, men substrängar är ofta en mer effektiv och enklare lösning. Det finns även flera metoder för att extrahera substrängar, som till exempel `find` och `substring`, men `substr` är den enklare att använda av de tre.

## Se även:

För mer information om `substr`, kan du kolla in dokumentationen för [C++ substr function](https://www.cplusplus.com/reference/string/string/substr/) eller läsa mer om [manipulering av strängar i C++](https://www.geeksforgeeks.org/substring-in-cpp/).