---
title:                "C: Wycinanie podciągów"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Zapewne wielu czytelników zastanawia się, po co w ogóle zajmować się wyodrębnianiem podłańcuchów w języku C. Czy nie jest to po prostu zbędne utrudnienie? Tymczasem wyodrębnianie podłańcuchów może być bardzo przydatne w różnych scenariuszach programistycznych. Przykładowo, możemy chcieć wyodrębnić wybrane elementy z jednego dłuższego łańcucha znaków, aby móc je przetworzyć lub wyświetlić w inny sposób. Dzięki wyodrębnianiu podłańcuchów możemy także efektywniej przeprowadzić operacje na tekście, co może znacznie przyśpieszyć nasz kod. W tym artykule pokażemy Wam kilka sposobów, jak wyodrębnić podłańcuchy w języku C.

## Jak to zrobić?

```C
#include <stdio.h>
#include <string.h>

int main() {

  // przykładowy łańcuch znaków
  char str[] = "Przykładowe zdanie";

  // wyodrębnienie podłańcucha od indeksu 4 do 13
  char substr[10];
  strncpy(substr, &str[4], 10); // skopiuj 10 znaków od indeksu 4
  substr[10] = '\0'; // zakończ łańcuch znakiem null
  printf("%s\n", substr); // wyświetli "kładowe z"

  // wyodrębnienie podłańcucha do pierwszego wystąpienia spacji
  char *space = strchr(str, ' '); // znajdź indeks pierwszej spacji
  if (space != NULL) {
    int index = (int)(space - str);  // konwertuj wskaźnik na indeks
    char substr2[index+1]; // utwórz tablicę o odpowiedniej długości
    strncpy(substr2, str, index); // skopiuj odpowiedni fragment łańcucha
    substr2[index] = '\0'; // zakończ łańcuch znakiem null
    printf("%s\n", substr2); // wyświetli "Przykładowe"
  }

  return 0;
}
```

W powyższym przykładzie wykorzystaliśmy funkcję `strncpy()`, która służy do kopiowania określonej liczby znaków z jednego łańcucha do drugiego. W przypadku wyodrębnienia pierwszego wystąpienia spacji, użyliśmy funkcji `strchr()`, która zwraca wskaźnik na pierwsze wystąpienie danego znaku w łańcuchu. Następnie wykorzystaliśmy konwersję wskaźnika na indeks, aby określić długość wyodrębnianego podłańcucha.

## Głębsze zagłębianie się w temat

Istnieje wiele innych funkcji, które mogą nam pomóc w wycinaniu lub wyodrębnianiu podłańcuchów w języku C. Przykładowo, możemy wykorzystać funkcję `strtok()` do podzielenia łańcucha na mniejsze części za pomocą określonego separatora. Warto również wspomnieć o funkcji `sscanf()`, która pozwala na wyodrębnienie podłańcucha, używając formatowania znane ze skanowania za pomocą `scanf()`. Dzięki temu możemy wygodnie określić co i gdzie chcemy wyciąć z łańcucha.

## Zobacz także

- [Dokumentacja języka C - funkcja strncpy()](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Dokumentacja języka C - funkcja strchr()](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm