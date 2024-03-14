---
date: 2024-01-26 03:37:55.773018-07:00
description: "Att ta bort citattecken fr\xE5n en str\xE4ng inneb\xE4r att skala bort\
  \ de d\xE4r irriterande dubbla eller enkla tecknen som omsluter v\xE5r text (' eller\
  \ \").\u2026"
lastmod: '2024-03-13T22:44:38.197279-06:00'
model: gpt-4-0125-preview
summary: "Att ta bort citattecken fr\xE5n en str\xE4ng inneb\xE4r att skala bort de\
  \ d\xE4r irriterande dubbla eller enkla tecknen som omsluter v\xE5r text (' eller\
  \ \").\u2026"
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort citattecken från en sträng innebär att skala bort de där irriterande dubbla eller enkla tecknen som omsluter vår text (' eller "). Programmerare gör ofta detta för att sanera input, lagra text i en databas eller förbereda strängar för vidare bearbetning utan störningen från citattecken.

## Hur man gör:
Här är ett rakt på sak sätt att kasta ut de där citattecknen till trottoarkanten i C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hej, 'Världen'!")";
    std::string utan_citat = remove_quotes(original);
    std::cout << utan_citat << std::endl;
    return 0;
}
```

Kör detta, och du får:

```
Hej, Världen!
```

Voila! Citattecknen har försvunnit.

## Djupdykning
Citattecken har varit en textplåga sedan datorernas gryning. Förr i tiden skulle du se programmerare mödosamt loopa igenom varje tecken för att filtrera bort de där citaten. Idag har vi `std::remove` i Standard Template Library (STL) för att göra det tunga lyftet.

Alternativ? Självklart! Du kan använda reguljära uttryck med `std::regex` för att rikta in dig på citat, men det är lite som att använda en slägga för att knäcka en nöt - kraftfullt, men kan vara överdrivet för enkla uppgifter. För de som föredrar senare smaker av C++, kanske ni experimenterar med `std::string_view` för icke modifierande angreppssätt.

När det gäller implementation, kom ihåg att `std::remove` faktiskt inte tar bort element från behållaren; det flyttar fram icke-borttagna element och returnerar en iterator bortom det nya slutet av omfånget. Det är därför vi behöver `erase`-metoden för att hugga av den oönskade svansen.

## Se Också
- C++ `std::remove` referens: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Mer om `std::string`-manipulering: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
