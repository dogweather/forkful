---
date: 2024-01-20 17:50:19.840685-07:00
description: "S\xE5 h\xE4r g\xF6r du: F\xF6rr i tiden slogs text ihop manuellt med\
  \ `+` eller `<<` operat\xF6rerna, vilket var oklart och ibland ineffektivt. C++20\
  \ introducerade\u2026"
lastmod: '2024-04-05T21:53:39.533891-06:00'
model: gpt-4-1106-preview
summary: "F\xF6rr i tiden slogs text ihop manuellt med `+` eller `<<` operat\xF6rerna,\
  \ vilket var oklart och ibland ineffektivt."
title: "Interpolera en str\xE4ng"
weight: 8
---

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
