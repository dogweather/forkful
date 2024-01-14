---
title:                "C++: Odczytywanie pliku tekstowego"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego czytanie pliku tekstowego jest ważne

Czytanie pliku tekstowego może być istotnym elementem w programowaniu w języku C++. Pliki tekstowe są szeroko wykorzystywane w aplikacjach do przechowywania danych, dlatego ważne jest umiejętne czytanie i przetwarzanie takiego typu plików. W tym artykule dowiesz się, dlaczego warto się nauczyć czytać pliki tekstowe w C++ oraz jak to zrobić.

## Jak czytać plik tekstowy w C++

Aby móc czytać plik tekstowy w C++, musimy najpierw otworzyć go przy użyciu obiektu typu `ifstream`. Dzięki temu skorzystamy z funkcjonalności klasy `ifstream`, która pozwala na odczytywanie danych z pliku. Następnie konieczne jest sprawdzenie, czy plik został poprawnie otwarty, co możemy zrobić korzystając z metody `is_open()`. Jeśli plik został otwarty, możemy przystąpić do czytania zawartości. Poniżej znajduje się przykład kodu w języku C++, który pokazuje jak to zrobić:

```C++
#include <iostream>
#include <fstream>

int main()
{
    // otwarcie pliku przy użyciu obiektu ifstream
    std::ifstream input;
    input.open("plik.txt");

    // sprawdzenie czy plik został otwarty
    if (input.is_open())
    {
        // deklaracja zmiennej, do której będziemy zapisywać dane z pliku
        std::string line;

        // czytanie pliku linia po linii, aż do końca
        while (std::getline(input, line))
        {
            // wyświetlanie zawartości linii na ekranie
            std::cout << line << std::endl;
        }

        // zamknięcie pliku po zakończeniu czytania
        input.close();
    }
    else // jeśli plik nie został otwarty poprawnie
    {
        std::cout << "Nie udało się otworzyć pliku." << std::endl;
    }

    return 0;
}
```

Wynikiem działania tego programu będzie wyświetlenie zawartości pliku tekstowego na ekranie. Warto zauważyć, że czytanie pliku odbywa się linia po linii, dzięki użyciu funkcji `getline()`.

## Głębszy wgląd w czytanie pliku tekstowego

Podczas czytania pliku tekstowego w C++, możemy skorzystać z różnych funkcji i metod do przetwarzania danych. Między innymi możemy wykorzystać funkcje do konwersji danych na inne typy, np. `stoi()` do zamiany tekstu na liczbę całkowitą. Warto również pamiętać o zarządzaniu pamięcią i zamykaniu pliku po zakończeniu czytania, aby uniknąć potencjalnych problemów.

W przypadku gdy plik zawiera dane w formacie innym niż tekstowy, możliwe jest wykorzystanie innych klas, takich jak `ifstream` czy `ofstream`, aby czytać i zapisywać dane binarne. Jest to jednak bardziej zaawansowany temat, który wymaga dodatkowej wiedzy i doświadczenia.

## Zobacz także

Jeśli chcesz pogłębić swoją wiedzę na temat czytania pliku tekstowego w C++, polecamy zapoznanie się z poniższymi źródłami:

- [Dokumentacja języka C++](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [Samouczek C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [Przykładowy kod na Githubie](https://github.com/subhamsoni94/File-handling-using-Cpp)

Dziękujemy za przeczytanie naszego arty