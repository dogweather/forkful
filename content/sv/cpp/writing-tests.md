---
title:                "Skriva tester"
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester handlar om att skapa kod som kontrollerar att annan kod fungerar som den ska. Programmerare gör detta för att upptäcka buggar tidigt, spara tid och säkerställa kodkvalitet.

## How to:
Testa vår kod med `assert` och generera rapporter. Upprätta först ett enkelt testfall:

```C++
#include <cassert>

int Add(int a, int b) {
    return a + b;
}

int main() {
    assert(Add(2, 3) == 5); // Testar vår Add-funktion
    // assert(Add(2, 2) == 5); // Detta skulle orsaka ett fel
    return 0;
}
```

Kör och se att det inte finns några felmeddelanden - vårt test passerade!

## Deep Dive
Tester började bli allmänt i 70-talet. Alternativ till `assert` inkluderar enhetstestramverk som Google Test. Dessa ramverk tillåter mer komplex tester och bättre rapportering.

## See Also
- [Google Test GitHub Repository](https://github.com/google/googletest)
- [C++ Reference on assert](http://cplusplus.com/reference/cassert/assert/)
