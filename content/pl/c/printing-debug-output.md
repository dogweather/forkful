---
title:                "C: Wyświetlanie wyników debugowania"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego?

Debugowanie jest jedną z najważniejszych części procesu pisania programów. Bez niego trudno jest znaleźć błędy i poprawić działanie naszego kodu. Jednym z potężniejszych narzędzi debugujących jest wyświetlanie informacji pomocniczych, które pomagają nam w prześledzeniu działania naszego programu i identyfikacji problemów. W tym artykule zobaczymy, dlaczego wyświetlanie debug output jest ważne i jak tego dokonać.

## Jak to zrobić?

Wyświetlanie debug output w języku C jest stosunkowo proste. Wystarczy wykorzystać funkcję `printf()` w celu wyświetlenia żądanych informacji. Przykładowy kod wyglądałby następująco:

```C
#include <stdio.h>

int main()
{
    int a = 5, b = 7;
    printf("Wartość a: %d, wartość b: %d.\n", a, b);
    return 0;
}
```

Powyższy kod wyświetli nam informację o wartościach zmiennych `a` i `b` na konsoli w trakcie działania programu. Jednak warto pamiętać, że wyświetlanie debug output powinno być używane tylko podczas debugowania, a nie w końcowej wersji produktu.

## Deep Dive

Za pomocą funkcji `printf()` możemy wyświetlić różnego rodzaju informacje, takie jak wartości zmiennych, tekst, czy nawet adresy pamięci. W celu odseparowania tych informacji możemy wykorzystać specjalne znaki formatujące, np. `%d` dla wartości całkowitych, `%f` dla liczb zmiennoprzecinkowych, czy `%s` dla tekstów.

Dodatkowo, możemy wykorzystać również inne funkcje, takie jak `sprintf()`, która pozwala na zapisanie wyświetlanych informacji do zmiennej, lub `fprintf()`, która zapisuje je do pliku zamiast na konsoli.

Pamiętajmy, żeby nie nadużywać wyświetlania debug output, ponieważ to może wpłynąć na wydajność naszego programu.

## Zobacz też

- ["Debugowanie w języku C"](https://pl.wikipedia.org/wiki/B%C5%82%C4%85d_programowania)
- ["Wyświetlanie debug output w języku Python"](https://realpython.com/python-debugging-pdb/#printing-values-to-understand-what-s-going-on)

Dzięki wyświetlaniu debug output możemy łatwiej zidentyfikować błędy w naszym kodzie i naprawić je szybciej. Pamiętajmy jednak, żeby używać go tylko podczas debugowania, a nie w końcowej wersji produktu. Miejmy również na uwadze wydajność naszego programu i unikajmy nadmiernego wyświetlania informacji pomocniczych.