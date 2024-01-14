---
title:    "C: Konwersja ciągu znaków na małe litery"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja ciągu znaków na małe litery jest często wykorzystywana w programowaniu do porównywania tekstów. W niektórych przypadkach może to również poprawić czytelność i estetykę kodu.

## Jak to zrobić

Jeśli pracujesz w języku C, konwersja ciągu znaków na małe litery jest prosta i wymaga zastosowania kilku funkcji bibliotecznych.

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
  // Definiowanie i inicjalizowanie ciągu znaków
  char str[] = "COdInG is FuN";
  
  // Wyświetlanie oryginalnego ciągu
  printf("Oryginalny ciąg: %s\n", str);
  
  // Konwersja ciągu na małe litery
  for (int i=0; i<strlen(str); i++) {
    str[i] = tolower(str[i]);
  }
  
  // Wyświetlanie zmienionego ciągu
  printf("Zmieniony ciąg: %s\n", str);
  
  return 0;
}
```

Output:

```
Oryginalny ciąg: COdInG is FuN
Zmieniony ciąg: coding is fun
```

## Deep Dive

W języku C funkcja `tolower()` z biblioteki `<ctype.h>` służy do konwersji pojedynczego znaku na małą literę. Jeśli chcemy zastosować to na całym ciągu, musimy użyć pętli, jak przedstawione w przykładzie powyżej. Innym sposobem jest użycie funkcji `strlwr()` z biblioteki `<string.h>`, która sama dokonuje konwersji całego ciągu.

Ważne jest również uważać na kodowanie znaków, ponieważ niektóre języki mają dodatkowe znaki diakrytyczne, które muszą być również uwzględnione podczas konwersji na małe litery. W takim przypadku warto sięgnąć po funkcje ze standardu Unicode, takie jak `towlower()`.

## Zobacz również

- [Dokumentacja funkcji tolower()](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/tolower.htm)
- [Dokumentacja funkcji strlwr()](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/strlwr.htm)
- [Standard Unicode](https://unicode.org/charts/)