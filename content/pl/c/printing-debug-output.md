---
title:                "Wydrukowanie wyniku debugowania"
html_title:           "C: Wydrukowanie wyniku debugowania"
simple_title:         "Wydrukowanie wyniku debugowania"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Debugowanie jest nieodłączną częścią tworzenia oprogramowania. Czasami, aby znaleźć błąd w kodzie, niezbędne jest wyświetlanie dodatkowych informacji. W tym artykule dowiesz się, dlaczego warto wykorzystywać funkcję wyświetlania informacji debugowych i jak to zrobić w języku C.

## Jak To Zrobić

Kodowanie informacji debugowych w języku C jest proste i wymaga wykorzystania funkcji `printf ()`. Aby wyświetlić wartość zmiennej w celu jej sprawdzenia, użyj specjalnej składni:

```C
printf("Wartość zmiennej x: %d \n", x);
```

W powyższym przykładzie, `%d` oznacza, że zostanie wyświetlona wartość zmiennej typu integer, a `\n` dodaje znak nowej linii po wyświetleniu. Możesz również wyświetlić więcej niż jedną zmienną lub łączyć wartości z tekstem:

```C
printf("Suma x i y: %d + %d = %d\n", x, y, x + y);
```

Jeśli potrzebujesz wyświetlić ścieżkę pliku lub nazwę funkcji, użyj funkcji `__FILE__` i `__FUNCTION__`:

```C
printf("Aktualnie wykonywany plik: %s\n", __FILE__);
printf("Aktualnie wykonywana funkcja: %s\n", __FUNCTION__);
```

Warto również pamiętać, że można użyć różnych specyfikatorów formatu, na przykład `%c` dla znaku, `%s` dla tekstu, `%f` dla liczb zmiennoprzecinkowych. Pełna lista jest dostępna w dokumentacji funkcji `printf ()`.

## Deep Dive

Wyświetlanie informacji debugowych może być bardzo pomocne w procesie debugowania, ale należy pamiętać, że nie należy używać go w kodzie produkcyjnym. Wyświetlanie dużej liczby informacji debugowych może znacznie spowolnić działanie programu.

Aby uniknąć nadmiernego wyświetlania informacji, można wykorzystać kompilatorowy wariant `#ifdef DEBUG` i `#endif` wokół kodu debugowania:

```C
#ifdef DEBUG
printf("Wartość zmiennej x: %d\n", x);
#endif
```

W ten sposób, kod debugowania będzie wykonany tylko w przypadku, gdy zostanie zdefiniowana flaga `DEBUG` podczas kompilacji.

Innym sposobem na wygodne debugowanie w języku C jest wykorzystanie narzędzi, takich jak gdb czy Valgrind, które oferują zaawansowane funkcjonalności do śledzenia błędów i analizowania pamięci.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o debugowaniu w języku C, polecamy zapoznanie się z poniższymi artykułami:

- [Debugowanie programów w języku C - Dokumentacja Microsoft](https://docs.microsoft.com/pl-pl/visualstudio/debugger/debugging-c-programs)
- [Debugowanie w języku C - Samouczek na Codecademy](https://www.codecademy.com/learn/learn-c/modules/learn-c-debugging)
- [Gdb - Dokumentacja oficjalna](https://www.gnu.org/software/gdb/documentation/)