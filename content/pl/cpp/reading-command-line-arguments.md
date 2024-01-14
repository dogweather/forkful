---
title:                "C++: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów uważa komunikację linii poleceń za przestarzałą i niepotrzebną. Jednakże, umiejętność czytania argumentów linii poleceń jest niezwykle przydatna w wielu sytuacjach. Na przykład, może się to okazać niezbędne, gdy tworzymy program w języku C++, który będzie uruchamiany przez system operacyjny lub konsolę. W tym artykule dowiesz się, dlaczego warto nauczyć się czytać argumenty linii poleceń w C++.

## Jak to zrobić

Do czytania argumentów linii poleceń w C++ możemy użyć funkcji "```int main(int argc, char *argv[])```". Zmienna "argc" przechowuje liczbę argumentów przekazanych do programu, zaś zmienna "argv" jest tablicą przechowującą same argumenty. Dzięki temu, możemy w łatwy sposób przetwarzać dane przekazane do programu.

Przykładowy kod:

```C++
#include <iostream>

int main(int argc, char *argv[]){
    std::cout << "Liczba przekazanych argumentów: " << argc << std::endl;
    for(int i = 0; i < argc; i++){
        std::cout << "Argument " << i+1 << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

Przykładowe wywołanie programu:

```bash
./program argument_1 argument_2 argument_3

Liczba przekazanych argumentów: 4
Argument 1: ./program
Argument 2: argument_1
Argument 3: argument_2
Argument 4: argument_3
```

## Głębsze spojrzenie

Warto zauważyć, że pierwszym elementem zmiennej "argv" jest nazwa samego programu (w powyższym przykładzie jest to "program"). Możemy tego wykorzystać, aby np. wyświetlić informację o programie lub odpowiednio przetworzyć argumenty.

Argumenty linii poleceń są również wykorzystywane przez wiele programów do przekazywania opcji. Przykładowo, jeśli uruchamiamy program z opcją "-h" lub "--help", program może wyświetlić pomoc lub listę dostępnych opcji.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o czytaniu argumentów linii poleceń w C++, polecamy zapoznać się z poniższymi artykułami:

- [Tutorial: Czytanie argumentów linii poleceń w C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Przykłady wykorzystania argumentów linii poleceń w programach C++](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [Dokumentacja biblioteki standardowej C++: Argumenty linii poleceń](https://en.cppreference.com/w/cpp/language/main_function)