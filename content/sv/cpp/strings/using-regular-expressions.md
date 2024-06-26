---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:10.945573-07:00
description: "Hur man g\xF6r: C++11 introducerade st\xF6d f\xF6r regulj\xE4ra uttryck\
  \ i standardbiblioteket, `<regex>`, vilket erbjuder ett robust ramverk f\xF6r str\xE4\
  ngs\xF6kning och\u2026"
lastmod: '2024-03-13T22:44:38.199198-06:00'
model: gpt-4-0125-preview
summary: "C++11 introducerade st\xF6d f\xF6r regulj\xE4ra uttryck i standardbiblioteket,\
  \ `<regex>`, vilket erbjuder ett robust ramverk f\xF6r str\xE4ngs\xF6kning och manipulation."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:
C++11 introducerade stöd för reguljära uttryck i standardbiblioteket, `<regex>`, vilket erbjuder ett robust ramverk för strängsökning och manipulation. Här är ett grundläggande exempel på hur man använder reguljära uttryck för att söka efter ett mönster inom en sträng:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hej, min e-post är exempel@exempel.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "E-post hittad!" << std::endl;
    } else {
        std::cout << "Ingen e-post hittad." << std::endl;
    }

    return 0;
}
```
**Exempelutmatning**
```
E-post hittad!
```

För mer komplexa manipulationer, som att ersätta mönster inom strängar, kan C++:s reguljära uttryck vara mycket användbara:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "Regnet i Spanien faller mestadels på slätten.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**Exempelutmatning**
```
R*gn*t * Sp*n**n f*ll*r m*st*d*ls p* sl*tt*n.
```

För programmerare som utforskar utanför standardbiblioteket är Boost Regex-biblioteket (`boost/regex.hpp`) ett populärt tredjepartsalternativ som erbjuder förbättrade regex-förmågor och prestandaoptimeringar, särskilt för komplexa mönster eller omfattande databearbetning:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost-biblioteken är roliga!";
    boost::regex expr("(\\w+)\\s(biblioteken)"); // Matchar "Boost-biblioteken"
    std::string fmt("GNU \\1"); // Ersätt med "GNU Boost"

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**Exempelutmatning**
```
GNU Boost är roliga!
```

Dessa exempel skrapar bara på ytan av C++:s kapaciteter med reguljära uttryck, och illustrerar grundläggande sökningar, mönstermatchning och ersättningar, antingen genom att använda standardbiblioteket eller förstärkt av Boosts kraftfulla regex-implementering.
