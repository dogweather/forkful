---
title:                "C++: Extrahering av substrängar"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig uppgift inom programmering, särskilt när man arbetar med textsträngar. Genom att kunna extrahera substrängar kan du få tillgång till specifika delar av en sträng som du behöver för att utföra olika operationer.

## Hur man gör det

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Skapar en sträng som vi vill extrahera en substräng från
    string namn = "Sofia Andersson";

    // Extraherar den första bokstaven i strängen
    string forstaBokstaven = namn.substr(0, 1);
    cout << "Första bokstaven i namnet är: " << forstaBokstaven << endl;

    // Extraherar de första fyra bokstäverna i strängen
    string forstaFyra = namn.substr(0, 4);
    cout << "De första fyra bokstäverna är: " << forstaFyra << endl;

    // Extraherar en del av strängen från en viss position
    string efternamn = namn.substr(6);
    cout << "Efternamnet är: " << efternamn << endl;

    return 0;
}
```

Output:
```
Första bokstaven i namnet är: S
De första fyra bokstäverna är: Sofia
Efternamnet är: Andersson
```

## Fördjupning

För att extrahera substrängar kan du använda funktionen `substr()` från standardbiblioteket `string`. Denna funktion tar två parametrar - startpositionen och längden på den önskade substrängen. Genom att välja rätt startposition och längd kan du få tillgång till olika delar av en sträng.

Funktionen `substr()` har också en överslagsparameter som låter dig extrahera en substräng från en viss position till slutet av strängen, vilket är användbart när du arbetar med variabler av olika längder.

## Se även

- [C++ Strings (Swedish)](https://www.zeolearn.com/magazine/working-with-strings-in-c)
- [String Substr function documentation (Swedish)](https://www.cplusplus.com/reference/string/string/substr/)