---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów z linii poleceń to sposób na wprowadzanie informacji do programu podczas jego uruchamiania. Programiści robią to, gdy chcą zwiększyć elastyczność i uniwersalność swoich aplikacji.

## Jak to zrobić:

W poniższym kodzie C++ zobaczysz, jak czytać argumenty z linii poleceń.

```C++
#include<iostream>
using namespace std;

int main(int argc, char *argv[])
{
    cout<<"Liczba argumentów: "<<argc<<endl;
    for(int i=0; i<argc; i++)
        cout<<"Argument "<<i+1<<": "<<argv[i]<<endl;

    return 0;
}
```

Przykładowe wyjście mogłoby wyglądać tak:

```Text
Liczba argumentów: 3
Argument 1: ./program
Argument 2: arg1
Argument 3: arg2
```

## Pogłębione informacje

Historia: Czytanie argumentów z linii poleceń jest stosowane od czasów korzeni języka,
C - stąd ta funkcjonalność została przekazana do C++.

Alternatywy: Istnieją biblioteki jak Boost.Program_options, które zapewniają bardziej zaawansowane możliwości parso- wania, ale są bardziej skomplikowane.

Implementacja: Parametry `argc` i `argv` są przekazywane do głównej funkcji (`main`). `argc` to liczba argumentów, a `argv` to tablica wskaźników do ciągów, które są argumentami.

## Zobacz także

1. Documentacja Boost.Program_options: https://www.boost.org/doc/libs/1_71_0/doc/html/program_options.html
2. Kompendium C++: https://cpp0x.pl/ksiazka/
3. cppreference.com: Odczyt argumentów z linii poleceń w C++: https://en.cppreference.com/w/cpp/language/main_function