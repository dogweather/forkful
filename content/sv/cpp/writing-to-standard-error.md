---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standard error (`stderr`) används för att rapportera fel och varningar. Programmerare gör detta för att separera vanlig output från felmeddelanden, vilket underlättar debuggning och loggning.

## How to:
Använd `cerr` för att skriva felmeddelanden. Här är ett exempel:

```C++
#include <iostream>

int main() {
    std::cerr << "Ett fel inträffade!" << std::endl;
    return 0;
}
```

Output: `Ett fel inträffade!`

## Deep Dive:
`stderr` är separerad från `stdout` (standard output) så att de kan hanteras olika. Historiskt sett tillät detta avledning av output till filer eller andra enheter. Alternativ till `std::cerr` inkluderar loggbibliotek eller `std::clog`. I Unix-system kan `stderr` och `stdout` avledas oberoende genom filbeskrivare 2 och 1.

## See Also:
- C++ Standard Library documentation: https://en.cppreference.com/w/cpp/io/cerr
- GNU C Library - Standard Streams: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- Effective use of C++ I/O: https://isocpp.org/wiki/faq/input-output
