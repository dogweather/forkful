---
title:    "C++: Zmiana wielkości litery w ciągu znaków"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Niedawno pewnie spotkaliście się z sytuacją, gdy musieliście zmienić napis na dużą literę w C++. Może to być nazwa użytkownika, tytuł artykułu lub dowolny inny ciąg znaków, który trzeba nagle "zabolować". Dlaczego warto poznać sposób na kapitalizację napisu w C++? Sprawia to, że nasze programy mają bardziej profesjonalny wygląd i są bardziej przyjemne dla oka.

## Jak to zrobić

W celu kapitalizacji napisu w C++, musimy użyć funkcji `toupper()` z biblioteki `<cctype>`. Poniżej znajduje się przykładowy kod, który pokazuje, jak to zrobić:

```C++
#include <iostream>
#include <string>
#include <cctype>

int main() {
  std::string napis = "witaj świecie";

  // Kapitalizacja pierwszej litery
  napis[0] = toupper(napis[0]);
  std::cout << napis << std::endl;

  // Kapitalizacja całego napisu
  for (int i = 0; i < napis.length(); i++) {
    napis[i] = toupper(napis[i]);
  }
  std::cout << napis << std::endl;

  return 0;
}
```

**Output**:
```
Witaj świecie
WITAJ ŚWIECIE
```

Jako wynik otrzymujemy kapitalizację pierwszej litery lub całego napisu, w zależności od naszych potrzeb.

## Głębszy wgląd

Co dzieje się wewnątrz funkcji `toupper()`? Głównym zadaniem tej funkcji jest zmiana podanej litery na jej odpowiednik z dużej litery. Jeśli jednak nasz napis zawiera polskie znaki, warto pamiętać o tym, że funkcja ta działa tylko na literach angielskiego alfabetu. Dlatego warto upewnić się, że nasz napis nie zawiera polskich znaków przed użyciem funkcji `toupper()`.

## Zobacz też

- Dokumentacja do funkcji `toupper()` w bibliotece `<cctype>`: https://www.cplusplus.com/reference/cctype/toupper/
- Przykłady zastosowań kapitalizacji w C++: https://www.programiz.com/cpp-programming/library-function/cctype/toupper