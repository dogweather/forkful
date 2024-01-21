---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:50:19.840685-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stringinterpolation innebär att blanda in variabler i en textsträng. Det gör kod läsbar och flexibel – vi slipper klumpigt plus-tecken (+) för att bygga meddelanden.

## Så här gör du:
```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    int age = 25;
    string name = "Erik";
    // Med modern C++ (C++20 och senare), kan vi använda std::format
    cout << std::format("Hej, jag heter {} och jag är {} år gammal.", name, age) << endl;
    // Utdata: Hej, jag heter Erik och jag är 25 år gammal.
    
    // För äldre C++ versioner användes stringstream
    stringstream ss;
    ss << "Hej, jag heter " << name << " och jag är " << age << " år gammal.";
    cout << ss.str() << endl;
    // Utdata är densamma
    return 0;
}
```

## Djupdykning
Förr i tiden slogs text ihop manuellt med `+` eller `<<` operatörerna, vilket var oklart och ibland ineffektivt. C++20 introducerade `std::format` som är inspirerat av Python's `str.format()` och ger en smidigare och mer läsbar syntaks. Alternativ finns för äldre C++ som `boost::format` eller att manuellt hantera `ostringstream`. När du interfolierar strängar, kom ihåg att effektiviteten kan variera beroende på metoden – `std::format` och `ostringstream` är generellt sett långsammare än `+` eller `<<`, men skillnaden är försumbar i små program.

## Se även
- C++20 [std::format documentation](https://en.cppreference.com/w/cpp/utility/format/format)
- [Stringstream library](https://en.cppreference.com/w/cpp/header/sstream)
- [Boost Format library](https://www.boost.org/doc/libs/1_75_0/libs/format/)
- [fmt library](https://fmt.dev/latest/index.html) (en portabel implementation av `std::format` för äldre C++)