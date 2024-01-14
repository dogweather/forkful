---
title:                "C: Zapisywanie ciągu wielkimi literami"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Wiele razy podczas pisania kodu w języku C musimy zmieniać pojawiające się w nim napisy. Jeden z przykładów może być zmiana pierwszej litery w wyrazie na wielką, co jest przydatne w celu prawidłowego formatowania tekstu. W tym blogu omówimy jak zmienić pierwszą literę napisu na wielką w języku C.

# Jak to zrobić

Aby zmienić pierwszą literę w wyrazie na wielką w języku C, musimy użyć pętli `for` w celu przejścia przez cały napis. Następnie używamy funkcji `toupper()` w celu zmiany pierwszej litery na wielką. Poniżej znajduje się przykładowy kod:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char word[] = "programowanie";
  int length = sizeof(word) / sizeof(word[0]);

  for(int i = 0; i < length; i++) {
    word[i] = toupper(word[i]);
  }

  printf("%s", word);
  
  return 0;
}
```

Po uruchomieniu powyższego kodu otrzymamy wyjście "PROGRAMOWANIE", gdzie pierwsza litera została zmieniona na wielką.

# Głębsza analiza

Funkcja `toupper()` jest dostępna w bibliotece `ctype.h` i służy do zmiany litery na jej odpowiednik wielką. Jest to przydatne narzędzie szczególnie podczas pracy z tekstem i jego formatowaniem. Pamiętajmy, że funkcja ta działa tylko na pojedynczej literze, więc aby zmienić cały napis, musimy użyć pętli `for`. W powyższym przykładzie używamy również funkcji `sizeof()` w celu określenia długości napisu, ponieważ jest to uniwersalny sposób na operacje na tablicach w języku C.

# Zobacz także

- [Dokumentacja funkcji toupper() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [Przydatne funkcje do pracy z tekstem w języku C](http://www.math.bas.bg/~iad/programs/c.functions.html)