---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text innebär att hitta specifika strängar i data och ändra dem till något annat. Programmerare gör detta för att snabbt korrigera, uppgradera eller ändra delar av koden.

## Hur gör man:
Här är hur vi gör sök och ersätt operationer med C++. Om man t.ex. vill ändra varje förekomst av ordet "katt" till "hund" i en textsträng:

```C++
#include <string>
#include <iostream>

int main() {
    std::string text = "Katten klättrar i träd. Katten är söt.";
    std::string from = "Katten";
    std::string to = "Hunden";

    size_t pos = text.find(from);
    while (pos != std::string::npos){
        text.replace(pos, from.length(), to);
		pos = text.find(from, pos + to.length());
    }

    std::cout << text;
    return 0;
}
```
När du kör programmet kommer outputen att vara:  
`Hunden klättrar i träd. Hunden är söt`.

## Djupdykning
Historiskt sett kom behovet att söka och ersätta text från textbehandling och dataanalys. C++ erbjuder flera sätt att göra detta på, som "std::replace", "std::regex_replace" eller skapande av egen funktion som vi gjorde i vårt exempel.

Ett alternativ till att använda inbyggda metoder är att använda bibliotek som Boost som erbjuder mer robusta metoder. Man kan också skapa egen funktion om man vill ha mer kontroll över processen. Om man gör det, bör märkas att ```std::string::find``` och ```std::string::replace``` metoder ändrar originalsträngen, vilket kan vara oönskat i vissa fall.

## Se även
Det finns massor att lära sig när det gäller C++. Här är några relaterade resources:

1. [C++ string library](https://en.cppreference.com/w/cpp/string)
2. [Boost Libraries for C++](https://www.boost.org/)

God programmeringslycka!