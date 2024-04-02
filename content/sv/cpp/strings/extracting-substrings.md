---
date: 2024-01-20 17:45:12.247581-07:00
description: "Att extrahera delstr\xE4ngar \xE4r processen att plocka ut specificerade\
  \ delar fr\xE5n en str\xE4ng. Programmerare g\xF6r detta f\xF6r att bearbeta, analysera\
  \ eller\u2026"
lastmod: '2024-03-13T22:44:38.198214-06:00'
model: gpt-4-1106-preview
summary: "Att extrahera delstr\xE4ngar \xE4r processen att plocka ut specificerade\
  \ delar fr\xE5n en str\xE4ng. Programmerare g\xF6r detta f\xF6r att bearbeta, analysera\
  \ eller\u2026"
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## What & Why? (Vad & Varför?)
Att extrahera delsträngar är processen att plocka ut specificerade delar från en sträng. Programmerare gör detta för att bearbeta, analysera eller modifiera textdata på ett flexibelt sätt.

## How to (Hur man gör)
Exempel på C++ med standardbiblioteket `<string>`:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string fullText = "Hallå världen! Välkommen till C++ programmering.";
    std::string substring = fullText.substr(6, 7); // Start vid index 6, ta 7 tecken.

    std::cout << substring << std::endl; // Skriver ut "världen"

    return 0;
}
```
Output:
```
världen
```

Använd `find` för att hitta den startpunkt där delsträngen ska extraheras:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string fullText = "Hej! Jag heter Erik och jag lär ut C++.";
    std::size_t startPos = fullText.find("Erik"); // Hittar startindex för "Erik".
    std::string name = (startPos != std::string::npos) ? fullText.substr(startPos, 4) : "";

    std::cout << name << std::endl; // Skriver ut "Erik"

    return 0;
}
```
Output:
```
Erik
```

## Deep Dive (Djupdykning)
Extrahering av delsträngar är inte nytt; det har varit en del av programmeringsspråk så länge som strängar har funnits. I C++ använder vi `substr` funktionen från `<string>` biblioteket.

Det finns alternativ till `substr`, som att använda iterators eller `copy` funktionen tillsammans med `back_inserter` för att skapa delsträngar. Valet mellan metoderna beror på situationen.

När det gäller implementationen utför `substr` en kopia av data från ursprungssträngen, vilket kan vara en kostsam operation för mycket långa strängar.

## See Also (Se Även)
- [cplusplus.com/reference/string/string/substr/](http://www.cplusplus.com/reference/string/string/substr/)
- [en.cppreference.com/w/cpp/string/basic_string/substr](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [cplusplus.com/reference/string/string/find/](http://www.cplusplus.com/reference/string/string/find/)
