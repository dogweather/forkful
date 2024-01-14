---
title:    "C++: Odczytywanie pliku tekstowego"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Witajcie programiści! Jeśli jesteście zainteresowani poznawaniem języka programowania C++, doskonale wiecie, jak ważne jest opanowanie umiejętności czytania plików tekstowych. Jest to kluczowy element w pracy z danymi, a także w wielu zastosowaniach, w których tekst jest podstawowym formatem danych. Dlatego warto zgłębić ten temat, aby stać się jeszcze bardziej efektywnym w tworzeniu aplikacji. Przeczytajcie dalej, aby dowiedzieć się, jak to zrobić i odkryć dodatkowe informacje na temat czytania plików tekstowych w C++.

## Jak to zrobić

Czytanie plików tekstowych w C++ jest prostsze, niż mogłoby się wydawać. Wystarczy skorzystać z kilku prostych funkcji i strumieni danych. Poniżej znajdziecie przykładowy kod z wykorzystaniem standardowej biblioteki C++.

```
#include <iostream>
#include <fstream>

int main() {
    // Otwórz plik tekstowy
    std::ifstream file("plik.txt");

    // Sprawdź, czy udało się otworzyć plik
    if (!file.is_open()) {
        std::cerr << "Nie udało się otworzyć pliku!" << std::endl;
        return 1;
    }

    // Odczytaj dane linijka po linijce
    std::string line;
    while (std::getline(file, line)) {
        // Wypisz każdą linię na ekran
        std::cout << line << std::endl;
    }

    // Zamknij plik
    file.close();

    return 0;
}
```

Aby zobaczyć działanie powyższego kodu, utwórzmy przykładowy plik tekstowy o nazwie "plik.txt" i zapiszmy w nim kilka linijek tekstu. Następnie skompilujmy i uruchommy powyższy kod. Otrzymamy na ekranie zawartość pliku tekstowego.

```
Pierwsza linijka tekstu
Druga linijka tekstu
Trzecia linijka tekstu
```

## Dogłębna analiza

W poprzedniej sekcji zaprezentowaliśmy prosty sposób na czytanie plików tekstowych w C++. Jednak warto poznać także inne techniki i możliwości, które dostarcza nam ten język programowania. Na przykład, możemy czytać nie tylko po jednej linijce tekstu, ale również po pojedynczych znakach. W tym celu możemy skorzystać z funkcji `std::ifstream::get()`. Inną funkcją, która może okazać się przydatna, jest `std::ifstream::read()`, która pozwala na odczytywanie danych w określonej liczbie znaków. Więcej informacji na temat tych funkcji oraz innych możliwości czytania plików tekstowych w C++ znajdziecie w dokumentacji biblioteki standardowej.

## Zobacz także

- [Dokumentacja biblioteki standardowej C++ na temat czytania plików tekstowych](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [Tutorial na temat obsługi plików tekstowych w C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [Przykład czytania pliku tekstowego w C++ z wykorzystaniem biblioteki boost](https://www.boost.org/doc/libs/1_76_0/libs/serialization/doc/tutorial.html#simplecase)