---
title:                "Radera tecken som matchar ett mönster"
html_title:           "C++: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort karaktärer som matchar ett mönster kan vara användbart när man vill rensa eller filtrera data. Det kan också vara ett sätt att effektivisera kod eller göra den mer läsbar.

## Så här gör du

För att ta bort karaktärer som matchar ett visst mönster i en sträng finns det flera olika tillvägagångssätt beroende på vad du vill uppnå och vilket språk du arbetar i. Nedan följer två exempel i C++.

```C++
// Exempel 1: Ta bort alla förekomster av bokstaven 'a' i en sträng
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str = "Hej på dig, alpaka!";
  
  // Loopa igenom strängen och jämför varje tecken med 'a'
  // Om karaktären inte är 'a', lägg till den i en ny sträng
  string ny_str;
  for (int i = 0; i < str.length(); i++) {
    if (str[i] != 'a') {
      ny_str += str[i];
    }
  }
  
  cout << ny_str; // Output: "Hej p dig, lpk!";
  
  return 0;
}
```

```C++
// Exempel 2: Ta bort alla siffror från en sträng
#include <iostream>
#include <string>
#include <cctype>

using namespace std;

int main() {
  string str = "Det är 2021, och snart är det 2022!";
  
  // Loopa igenom strängen och kontrollera om varje tecken är en siffra
  // Om inte, lägg till tecknet i en ny sträng
  string ny_str;
  for (int i = 0; i < str.length(); i++) {
    if (!isdigit(str[i])) {
      ny_str += str[i];
    }
  }
  
  cout << ny_str; // Output: "Det är , och snart är det !";
  
  return 0;
}
```

## Djupdykning

För att ta bort karaktärer som matchar ett visst mönster kan man också använda sig av olika funktioner och metoder beroende på det specifika språket. I C++ finns till exempel en funktion som heter erase() som kan användas för att ta bort specifika tecken eller en del av en sträng.

Det finns också olika sätt att matcha ett mönster, som till exempel att använda regex (regular expressions), vilket kan vara användbart om man vill ta bort mer komplexa mönster av karaktärer.

## Se även

- cplusplus.com/reference/cstring/strtok/
- geeksforgeeks.org/clearing-the-input-buffer-in-cc/
- std::string::erase - C++ Reference