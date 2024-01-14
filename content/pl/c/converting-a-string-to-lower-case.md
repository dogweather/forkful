---
title:                "C: Konwertowanie ciągu znaków na małe litery"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

### Dlaczego

Gdy piszesz programy w języku C, często potrzebujesz manipulować tekstem. Jednym z często spotykanych zadań jest konwersja wszystkich liter w ciągu znaków na małe litery. W tym wpisie dowiesz się, dlaczego ten proces jest ważny i jak możesz go zaimplementować w swoim kodzie.

### Jak to zrobić

Istnieje kilka sposobów na konwersję ciągu znaków na małe litery w języku C. Jednym z najprostszych jest użycie funkcji `tolower()` z biblioteki standardowej `ctype.h`. Przyjmie ona jako argument pojedynczy znak i zwróci jego małą literę.

Przykładowy kod wykorzystujący tę funkcję może wyglądać następująco:

```C
#include <stdio.h>
#include <ctype.h>

int main(void) {
  char str[] = "POLSKI TEKST";
  for (int i = 0; str[i] != '\0'; i++) { // pętla przechodząca przez każdy znak w ciągu
      str[i] = tolower(str[i]); // użycie funkcji tolower() do konwersji na małe litery
  }
  printf("%s", str); // wyświetlenie zmodyfikowanego ciągu
  return 0;
}

```

Po uruchomieniu kodu, otrzymamy na wyjściu "polski tekst". 

### Głębsza analiza

Funkcja `tolower()` zwraca liczbę całkowitą, dlatego aby otrzymać zmodyfikowany znak w formie pojedynczego bajtu, musimy użyć rzutowania na typ `char`. 

Pamiętaj również, że kod ASCII dla małych liter różni się od kodu dla dużych liter o 32. Dlatego nie możemy po prostu odjąć od wartości znaku 32, aby otrzymać jego małą literę. Dlatego funkcja `tolower()` jest bardzo przydatna, ponieważ sama zajmuje się uwzględnieniem różnic między kodami ASCII.

Jeśli potrzebujesz konwertować całe wyrazy na małe litery, a nie pojedyncze znaki, warto użyć funkcji `strcpy()` i `strlwr()` z biblioteki `string.h`. Pierwsza skopiuje oryginalny ciąg do nowej zmiennej, a druga dokona konwersji na małe litery.

Należy także pamiętać, że funkcja `tolower()` operuje tylko na literach alfabetu, a inne znaki pozostawia bez zmian. Jeśli chcesz uwzględnić wszystkie możliwe znaki, warto użyć funkcji `isalpha()` z biblioteki `ctype.h`, aby sprawdzić, czy dany znak jest literą.

### Zobacz także
- [Dokumentacja funkcji tolower()](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Konwersja liter alfabetu łacińskiego na małe litery w języku C](https://stackoverflow.com/questions/1926423/converting-char-in-c-from-uppercase-to-lowercase)