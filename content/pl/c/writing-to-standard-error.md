---
title:                "C: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się dlaczego niektóre programy wyświetlają błędy na ekranie, a inne wysyłają je do standardowego wyjścia? Czy wiesz, że można wiele zyskać, pisząc błędy do standardowego wyjścia? W tym artykule dowiesz się dlaczego warto pisać do standardowego wyjścia i jak to zrobić.

## Jak To Zrobić

Aby napisać błąd do standardowego wyjścia w języku C, musimy użyć funkcji `fprintf` i przekazać jej `stderr` jako pierwszy argument. Poniżej znajduje się przykładowy kod, który wypisuje wiadomość o błędzie na ekran:

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "Wystąpił błąd!");
  return 0;
}
```

Po uruchomieniu tego programu, zobaczymy wyjście na ekranie:

```
Wystąpił błąd!
```

Możemy również przekazać dodatkowe argumenty do funkcji `fprintf`, aby wypisać bardziej szczegółową informację o błędzie. Przykładowo:

```C
#include <stdio.h>

int main() {
  int x = 5, y = 0;
  if (y == 0) {
    fprintf(stderr, "Dzielenie przez 0: x = %d, y = %d", x, y);
  }
  return 0;
}
```

W takim przypadku, zobaczymy na ekranie informację o dzieleniu przez 0, wraz z wartościami zmiennych wykorzystanych w operacji:

```
Dzielenie przez 0: x = 5, y = 0
```

## Pogłębiona Analiza

Teraz, kiedy wiesz jak pisać błędy do standardowego wyjścia, możesz zastanawiać się dlaczego jest to ważne? Dlaczego niektóre programy wypisują błędy na ekran, a inne korzystają z standardowego wyjścia? Głównym powodem jest to, że standardowe wyjście jest buforowane, co oznacza, że wiadomości tam wypisane zostały zapisane w pamięci do momentu, gdy program zakończy swoje działanie. Dzięki temu, błędy zostają zapisane nawet w przypadku, gdy program kończy się awarią. Co więcej, dzięki wykorzystaniu standardowego wyjścia, programista może kontrolować w jaki sposób i gdzie błędy są zapisywane, co ułatwia późniejsze debugowanie i naprawianie kodu.

## Zobacz również
- [Instrukcja fprintf w języku C](https://www.geeksforgeeks.org/fprintf-in-c/)
- [Wyjaśnienie pojęcia buforowania](https://www.tutorialspoint.com/What-is-bufferring-in-C-Cplusplus)
- [Porównanie standardowego wyjścia i standardowego błędu](https://www.oreilly.com/library/view/learning-the-bash/9781491975329/ch04.html)