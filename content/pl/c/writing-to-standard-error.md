---
title:                "Pisanie do standardowego błędu"
html_title:           "C: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego wyjścia błędu może wydawać się niepotrzebnym krokiem w procesie pisania aplikacji w języku C. Jednakże, jest to bardzo ważne narzędzie, które pomaga w diagnozowaniu błędów i utrzymaniu czystego i czytelnego kodu. W tym artykule dowiesz się dlaczego warto pisać do standardowego wyjścia błędu oraz jak to zrobić w praktyce.

## Jak to zrobić

Przedstawimy teraz przykładowy kod w języku C, który wykorzystuje pisanie do standardowego wyjścia błędu. Załóżmy, że mamy prostą funkcję, która dzieli dwie liczby i zwraca wynik. Chcemy wypisać błąd, jeśli użytkownik poda jako drugą liczbę 0, aby uniknąć dzielenia przez zero.

```
#include <stdio.h>

int divide(int x, int y) {
  if (y == 0) {
    fprintf(stderr, "Nie można dzielić przez zero!\n");
    return -1; // zwracamy kod błędu
  }
  return x / y;
}

int main() {
  int result = divide(10, 0);
  printf("Wynik: %d\n", result);
  return 0;
}
```

W powyższym przykładzie używamy funkcji `fprintf` z parametrem `stderr`, co oznacza, że tekst zostanie wypisany do standardowego wyjścia błędu. Jest to alternatywny sposób na wypisywanie informacji, gdyż pozwala utrzymać czytelność kodu, a także pozwala na przekazanie dodatkowych informacji na temat błędu.

Po uruchomieniu powyższego programu otrzymamy następujący wynik:

```
Nie można dzielić przez zero!
Wynik: -1
```

Widzimy, że pomyślnie wywołaliśmy wypisywanie błędu do standardowego wyjścia błędu oraz otrzymaliśmy poprawną wartość błędu zwróconą przez funkcję `divide`.

## Deep Dive

Standardowe wyjście błędu jest jednym z trzech standardowych strumieni wyjściowych w języku C, obok standardowego wyjścia i standardowego wyjścia błędu. W odróżnieniu od standardowego wyjścia, które służy do wypisywania danych dla użytkownika, standardowe wyjście błędu służy do wypisywania informacji o błędach. Oznacza to, że jest to bardzo ważne narzędzie do debugowania kodu i poprawnego działania aplikacji.

Jedną z najważniejszych korzyści związanych z pisanie do standardowego wyjścia błędu jest możliwość przekazywania informacji o błędach bez przerywania standardowego wyjścia. Dzięki temu, użytkownik może nadal otrzymywać informacje i wyniki swoich działań, a jednocześnie otrzymać ostrzeżenia o ewentualnych błędach.

Jest też możliwość przekierowania standardowego wyjścia błędu do pliku, co może być przydatne w sytuacji, gdy nie chcemy, aby informacje o błędach wyświetlały się na ekranie użytkownika.

## Zobacz również

- [Pisanie do standardowego wyjścia błędu w języku C](https://www.cplusplus.com/reference/cstdio/stderr/)
- [Obsługa błędów w języku C](https://www.cprogramming.com/tutorial/c/lesson17.html)
- [Strumienie wyjściowe w języku C](https://www.tutorialspoint.com/cprogramming/c_input_output.htm)