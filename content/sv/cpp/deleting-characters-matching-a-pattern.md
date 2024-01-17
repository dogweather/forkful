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

## Vad & Varför?
Att ta bort tecken som matchar ett mönster är en vanlig manipulation bland programmerare. Genom att ta bort oönskade tecken kan vi rensa och anpassa datan enligt våra behov.

## Så här:
Om vi till exempel har en sträng "Hej hej värld" och vill ta bort alla mellanslag, kan vi använda oss av en for-loop för att iterera igenom varje tecken i strängen och ta bort mellanslaget om det matchar " ".

```C++
#include <iostream>
#include <string>

int main()
{
  std::string str = "Hej hej värld";

  // loopar igenom strängen och tar bort mellanslaget
  for (int i = 0; i < str.length(); i++)
  {
    if (str[i] == ' ')
    {
      str.erase(i, 1);
    }
  }

  std::cout << str << std::endl;
  // output: Hejhejvärld
  return 0;
}
```

## Deep Dive:
Att ta bort tecken som matchar ett specifikt mönster kan vara användbart när vi behöver filtrera eller förbereda data innan vi bearbetar den vidare. Det finns också andra alternativ för att rensa datan, som till exempel att använda en reguljär uttryck eller en inbyggd funktion för att ta bort tecken från en sträng.

När vi tar bort tecken från en sträng kan vi också ange vilken del av strängen vi vill börja på och hur många tecken vi vill ta bort. Detta ger oss en större flexibilitet när det kommer till manipulering av textdata.

## Se även:
- [Reguljära uttryck i C++](https://www.regular-expressions.info/gcc.html)
- [Inbyggda strängfunktioner i C++](https://www.cplusplus.com/reference/string/string/)
- [C++ Styleguide](https://google.github.io/styleguide/cppguide.html)