---
title:    "C++: Drukowanie wyników debugowania"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy, podczas programowania w C++, spotykamy się z problemami podczas wykonywania kodu. Nie zawsze wiemy, dlaczego program nie działa poprawnie lub zwraca nieoczekiwane wyniki. Właśnie w takich sytuacjach wypisywanie informacji diagnostycznych na konsolę może okazać się nieocenioną pomocą. Dzięki nim, będziemy mogli zobaczyć, co dzieje się w naszym programie i szybko znaleźć źródło problemu.

## Jak to zrobić

Istnieje kilka sposobów na wypisywanie informacji diagnostycznych podczas programowania w C++. Jednym z najprostszych jest użycie funkcji `cout` z biblioteki standardowej `iostream`. Wywołując tę funkcję, możemy przekazać jej dowolną wartość lub zmienną, a następnie wyświetlić ją na konsoli.

```C++
#include <iostream>

int main() {
  int a = 5;
  std::cout << a << "\n";
}
```

W powyższym przykładzie, zmienna `a` zostanie wypisana na konsoli, dzięki czemu będziemy mogli sprawdzić jej wartość. Oprócz funkcji `cout`, możemy także skorzystać z funkcji `printf` z biblioteki `cstdio`, która pozwala nam na większą kontrolę nad formatem wyświetlania danych.

## Rzucamy się głębiej

Wypisywanie informacji diagnostycznych może być także przydatne, kiedy musimy monitorować działanie programu w różnych miejscach. W takim przypadku, możemy skorzystać z makra `DEBUG`, które pozwala nam na włączenie i wyłączenie trybu debugowania w zależności od ustawienia kompilatora.

```C++
#include <iostream>

#ifdef DEBUG
  #define DEBUG_LOG(x) do { std::cout << x << "\n"; } while (0)
#else
  #define DEBUG_LOG(x)
#endif

int main() {
  int a = 5;
  DEBUG_LOG("Wartość zmiennej a: " << a);
  // reszta kodu
}
```

Dzięki takiemu podejściu, możemy łatwo włączać i wyłączać wypisywanie informacji diagnostycznych w zależności od potrzeb.

## Zobacz także

* [Podstawy języka C++](https://pl.wikipedia.org/wiki/C%2B%2B)
* [Artykuł o debugowaniu w języku C++](https://programistamag.pl/debugowanie-w-c-co-warto-o-nim-wiedziec/)