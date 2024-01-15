---
title:                "Stor bokstavsättning av en sträng"
html_title:           "C++: Stor bokstavsättning av en sträng"
simple_title:         "Stor bokstavsättning av en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Är du trött på att ha ojämnt kapitaliserade strängar i ditt C++-program? Vill du ha en mer estetiskt tilltalande output? Då är det dags att lära dig hur man kapitaliserar strängar i C++.

## Hur man gör

För att kapitalisera en sträng i C++ finns det olika metoder beroende på dina behov. Nedan följer några enkla exempel med kod och utskrifter.

```C++
#include <iostream> 
// inkluderar standardbiblioteket för data-input och output

#include <algorithm> 
// inkluderar biblioteket för att använda funktioner för textmanipulering

using namespace std; 
// definierar standard namespace för att slippa skriva "std::" före varje standardfunktion

int main() 
{ 
    string s = "en sträng"; // definierar en sträng med värde
    
    // Metod 1: Använda transform() funktionen från <algorithm> biblioteket
    transform(s.begin(), s.end(), s.begin(), ::toupper);
    // för att kapitalisera alla bokstäver i strängen
        
    cout << "Metod 1: " << s << endl;  // skriver ut kapitaliserad sträng

    // Metod 2: Använda toupper() funktionen från <ctype> biblioteket
    for (int i = 0; i < s.length(); i++) 
    { 
        s[i] = toupper(s[i]); 
        // går igenom varje bokstav och kapitaliserar den 
    } 
                
    cout << "Metod 2: " << s << endl;  // skriver ut kapitaliserad sträng

    // Metod 3: Använda islower() och toupper() funktionerna från <cctype> biblioteket
    for (int i = 0; i < s.length(); i++) 
    { 
        if(islower(s[i]))
        {
            s[i] = toupper(s[i]);
        }
        // kollar om bokstaven är en gemener och om den är det, kapitaliserar den
    } 
            
    cout << "Metod 3: " << s << endl;  // skriver ut kapitaliserad sträng

    return 0; 
} 
```

**Output:** \
Metod 1: EN STRÄNG \
Metod 2: EN STRÄNG \
Metod 3: EN STRÄNG

## Djupdykning

Genom att inkludera <algorithm> och <ctype> biblioteken, får du tillgång till funktioner för textmanipulering. Transform() funktionen är speciellt användbar när man vill utföra ett åtgärder på alla element i en container, medan toupper() och islower() funktionerna är mer specifika för hantering av bokstäver. Genom att kombinera dessa funktioner med en loop kan du enkelt kapitalisera en hel sträng. Man kan också använda den inbyggda funktionen to_string() för att konvertera numeriska värden till strängar och därmed kapitalisera siffror i en sträng.

## Se även

- [C++ string manipulation](https://www.geeksforgeeks.org/string-manipulation-in-c/#:~:text=transform()%20function%20of%20algorithm,passed%20as%20an%20input%20in%20transform().)
- [toupper() function in C++](https://www.geeksforgeeks.org/toupper-function-in-c/)
- [islower() function in C++](https://www.geeksforgeeks.org/islower-function-in-c/)