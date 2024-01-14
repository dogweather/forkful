---
title:                "C++: Odczytywanie argumentów wiersza poleceń"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czym są argumenty wiersza poleceń i dlaczego warto nauczyć się ich czytać w programowaniu C++? Argumenty wiersza poleceń pozwalają na przekazanie danych do programu przy uruchamianiu go z poziomu konsoli. Jest to przydatne w wielu sytuacjach, na przykład w przypadku konfiguracji programu lub przekazywania dużych ilości danych. W tym artykule dowiesz się, jak czytać argumenty wiersza poleceń w C++ i jak wykorzystać je w swoich projektach.

## Jak to zrobić

C++ oferuje prosty sposób na czytanie argumentów wiersza poleceń dzięki funkcji `argc` oraz `argv`. Poniższy przykład pokazuje, jak wykorzystać te funkcje w prostym programie, który wyświetla podane argumenty w konsoli.

```C++
#include <iostream>

int main(int argc, char* argv[]) {

  for (int i = 0; i < argc; i++) {
    std::cout << "Argument " << i << ": " << argv[i] << std::endl;
  }

  return 0;
}
```

Dzięki funkcji `argc` mamy możliwość odczytania ilości podanych argumentów, a funkcja `argv` zwraca tablicę ze wszystkimi argumentami (włącznie z nazwą pliku wykonywalnego). Przykład ten można łatwo modyfikować, np. dodając instrukcje warunkowe, aby odpowiednio obsłużyć różne ilości argumentów lub wyświetlić tylko wybrane argumenty.

Przykładowe wywołanie programu w terminalu może wyglądać następująco:
```
./myprogram arg1 "argument 2" "trzeci argument"
```

A jego wynikiem będzie:
```
Argument 0: ./myprogram
Argument 1: arg1
Argument 2: argument 2
Argument 3: trzeci argument
```

Dzięki tej wiedzy możesz łatwo rozwinąć swoje umiejętności programistyczne i wykorzystywać argumenty wiersza poleceń w praktyce.

## Głębszy zanurzenie

W przypadku bardziej zaawansowanych zastosowań, można wykorzystać dodatkowe biblioteki, takie jak `boost::program_options`, które oferują rozszerzone możliwości dotyczące argumentów wiersza poleceń, np. walidację danych czy obsługę opcji.

## Zobacz także

1. Dokumentacja C++ dla funkcji `argc` i `argv`: https://en.cppreference.com/w/cpp/language/main_function
2. Przykładowe wykorzystanie argumentów wiersza poleceń w C++: https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm
3. Biblioteka `boost::program_options`: https://www.boost.org/doc/libs/1_66_0/doc/html/program_options.html