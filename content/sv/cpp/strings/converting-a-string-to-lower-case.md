---
date: 2024-01-20 17:38:27.555063-07:00
description: "Konvertera en str\xE4ng till gemener (sm\xE5 bokst\xE4ver) inneb\xE4\
  r att omvandla alla stora bokst\xE4ver i en textstr\xE4ng till deras motsvarande\
  \ sm\xE5 bokst\xE4ver.\u2026"
lastmod: '2024-02-25T18:49:36.511312-07:00'
model: gpt-4-1106-preview
summary: "Konvertera en str\xE4ng till gemener (sm\xE5 bokst\xE4ver) inneb\xE4r att\
  \ omvandla alla stora bokst\xE4ver i en textstr\xE4ng till deras motsvarande sm\xE5\
  \ bokst\xE4ver.\u2026"
title: "Konvertera en str\xE4ng till gemener"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Konvertera en sträng till gemener (små bokstäver) innebär att omvandla alla stora bokstäver i en textsträng till deras motsvarande små bokstäver. Programmerare gör detta för att standardisera data, underlätta jämförelser och sökningar utan att oroa sig för blandade skiftlägen.

## How to (Hur man gör):
I C++ använder vi biblioteksfunktioner som `tolower`:

```C++
#include <iostream>
#include <algorithm>
#include <cctype>

int main() {
    std::string text = "Hej Världen!";
    std::transform(text.begin(), text.end(), text.begin(), 
        [](unsigned char c){ return std::tolower(c); });
    
    std::cout << text << std::endl; // Output: hej världen!
    
    return 0;
}
```

Och här är en alternativ metod:

```C++
#include <iostream>
#include <cctype>

int main() {
    std::string text = "Hej Världen!";
    for (char &c : text) {
        c = std::tolower(static_cast<unsigned char>(c));
    }

    std::cout << text << std::endl; // Output: hej världen!
    
    return 0;
}
```

## Deep Dive (Fördjupning):
I historiens begynnelse behandlades text ofta i endast stora bokstäver. När datorsystem utvecklades, blev behovet av att hantera både stora och små bokstäver uppenbart. Funktioner som `tolower` kom för att göra omvandlingen enkel och effektiv.

Det finns flera sätt att konvertera strängar till gemener i C++. Standardbiblioteket ger oss `tolower`, men lite omsorg krävs - tecken måste typomvandlas till `unsigned char` för att undvika odefinierat beteende på tecken med negativa värden.

Alternativt kan man använda bibliotek som Boost för att hantera konverteringar med bättre support för utf-8 och andra teckenuppsättningar. Moderna C++ versioner (C++11 och framåt) hanterar strängar och tecken på ett mer sofistikerat sätt, vilket gör det möjligt att arbeta med olika språk och specialtecken.

För någon som bryr sig om prestanda, det kan vara intressant att veta att att iterera över en sträng och konvertera tecken för tecken själv är snabbare än att använda `std::transform` med en lambda-funktion. Detta på grund av att lambda kan introducera något overhead.

## See Also (Se även):
- [C++ reference for std::tolower](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [C++ reference for std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Boost String Algo Library](https://www.boost.org/doc/libs/release/libs/algorithm/string/)
- [Unicode support in C++](http://www.unicode.org/versions/latest/)
