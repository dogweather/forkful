---
title:    "C: Konwertowanie ciągu znaków na małe litery"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Dlaczego

Konwersja ciągu znaków na małe litery jest ważnym aspektem programowania, szczególnie w języku C. Pozwala to na łatwiejsze porównywanie i przetwarzanie tekstu, co może być przydatne w różnych sytuacjach, na przykład w tworzeniu interfejsów użytkownika lub filtrowaniu danych. W tym artykule dowiesz się, jak dokonać konwersji ciągu znaków na małe litery w języku C.

# Jak to zrobić

Do konwersji ciągu znaków na małe litery w języku C możemy użyć funkcji `tolower()` z biblioteki standardowej `ctype.h`. Poniżej przedstawiamy przykładową implementację tej funkcji i jej wywołanie:

```C
#include <stdio.h>
#include <ctype.h>

// Funkcja konwertująca ciąg znaków na małe litery
void toLowerCase(char* str) {
  int i = 0;
  while (str[i]) {
    str[i] = tolower(str[i]);
    i++;
  }
}

int main() {
  // Przykładowy ciąg znaków
  char str[] = "Hello World";

  // Wywołanie funkcji
  toLowerCase(str);

  // Wyświetlenie wyniku
  printf("Wynik: %s\n", str);

  return 0;
}
```

Kod ten najpierw importuje bibliotekę `stdio.h`, która zawiera funkcję `printf()`, oraz bibliotekę `ctype.h`, która zawiera funkcję `tolower()`. Następnie definiujemy funkcję `toLowerCase()`, która przyjmuje jako argument wskaźnik na ciąg znaków. Funkcja ta iteruje po wszystkich znakach ciągu i zamienia je na ich odpowiedniki w postaci małych liter za pomocą funkcji `tolower()`. W głównej funkcji `main()` tworzymy przykładowy ciąg znaków, wywołujemy naszą funkcję i wyświetlamy wynik na ekranie.

Przykładowy wynik działania programu:

```
Wynik: hello world
```

# Deep Dive

Podczas konwersji ciągu znaków na małe litery w języku C należy pamiętać o kilku rzeczach. Po pierwsze, funkcja `tolower()` nie zmienia znaków niealfanumerycznych, więc jeśli w ciągu występują znaki specjalne, nadal będą one wyświetlane duże. Jeśli chcesz zamienić również te znaki na małe, możesz użyć pętli i wywołać funkcję `tolower()` dla każdego znaku osobno. Po drugie, konwersja na małe litery może zależeć od ustawień regionalnych systemu operacyjnego, więc nie zawsze możemy być pewni, że nasz kod będzie działał poprawnie na wszystkich urządzeniach.

# Zobacz także

- [Funkcja `tolower()` w dokumentacji języka C (ang.)](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Inne sposoby konwersji ciągu znaków na małe litery w języku C (ang.)](https://www.geeksforgeeks.org/convert-string-lower-case-using-c/)
- [Dokumentacja biblioteki `ctype.h` (ang.)](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)