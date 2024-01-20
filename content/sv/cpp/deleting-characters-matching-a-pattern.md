---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Att radera tecken som matchar ett mönster innebär att man tar bort alla instanser av ett visst tecken eller en teckensekvens från en sträng. Programmörer gör det för att rensa upp data, genom att bli av med oönskade eller onödiga element.

### Hur gör man:
För att illustrera hur man raderar tecken som matchar ett mönster, används std::remove_if och std::string::erase metoder. Här är ett exempel:

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string str = "Hej, .!rekne15:2019*Världen";

    str.erase(std::remove_if(str.begin(), str.end(), ::ispunct), str.end());

    std::cout << str;
    return 0;
}
```

I det här exemplet, tar vi bort alla skiljetecken från strängen. Outputen blir:

```
Hej rekne152019Världen
```

### Djupdykning
Funktionerna std::remove_if och std::string::erase används i samband sedan C++98. 

Ett alternativ till detta är att skriva en egen loop för att iterera över strängen och radera tecken som matchar ett visst mönster vilket kan leda till mer komplex kod.

Implementeringen av dessa funktioner i C++ gör att borttagning av tecken är både snabbt och enkelt. std::remove_if flyttar element som inte ska raderas till början av strängen och returnerar en iterator till första oönskade elementet. Sedan använder std::string::erase för att radera oönskade tecken.

### Se också
För mer information om att arbeta med strängar i C++, se följande källor:

- [C++ Standard Library: String](https://en.cppreference.com/w/cpp/string/basic_string)
- [Cplusplus.com: String Library](http://www.cplusplus.com/reference/string/string/)