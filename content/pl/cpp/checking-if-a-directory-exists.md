---
title:                "C++: Sprawdzanie, czy katalog istnieje."
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie czy katalog istnieje jest ważną częścią programowania w C++. Stwarza to możliwość sprawdzenia, czy dana ścieżka jest poprawna i czy możemy z niej korzystać w naszym kodzie.

## Jak to zrobić

Jeśli chcesz sprawdzić, czy dany katalog istnieje, musisz użyć funkcji `std::filesystem::exists()`. Najpierw musisz jednak zaimportować odpowiednią bibliotekę za pomocą `#include <filesystem>`. Następnie w bloku `main()` możesz stworzyć zmienną typu `bool` i przypisać jej wartość zwracaną przez funkcję `exists()`. Na przykład:

```C++
#include <iostream>
#include <filesystem>

int main() {
    bool exists = std::filesystem::exists("sciezka/do/katalogu");

    if (exists) {
        std::cout << "Katalog istnieje.";
    } else {
        std::cout << "Katalog nie istnieje.";
    }

    return 0;
}
```

W powyższym przykładzie najpierw zaimportowaliśmy bibliotekę `<filesystem>`, następnie w bloku `main()` stworzyliśmy zmienną `exists` typu `bool`, do której przypisaliśmy wartość zwracaną przez funkcję `exists()` z podaniem ścieżki do katalogu. Następnie w zależności od wartości zmiennej `exists` wyświetlamy odpowiedni komunikat.

## Pogłębiona analiza

Sprawdzanie, czy katalog istnieje, może być przydatne w wielu przypadkach. Na przykład przy tworzeniu programów, które w swoim działaniu korzystają z plików zapisanych w określonych lokalizacjach. Dzięki wykorzystaniu funkcji `exists()` możemy upewnić się, że kod wykonamy tylko w przypadku, gdy dana ścieżka jest poprawna. Jest to szczególnie ważne w projekcie, w którym pracuje wiele osób, ponieważ może zdarzyć się, że ktoś wprowadził niepoprawną ścieżkę do pliku, przez co cały program przestanie działać.

Ponadto, funkcja `exists()` może być również wykorzystana do sprawdzenia, czy dany katalog został utworzony przez nasz program. W przypadku, gdy chcemy zapisać plik w określonej lokalizacji, warto najpierw sprawdzić, czy dany katalog istnieje. Jeśli nie, możemy go utworzyć za pomocą funkcji `std::filesystem::create_directory()`.

## Zobacz również

- [Dokumentacja C++ - std::filesystem::exists()](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Artykuł na temat tworzenia i usuwania katalogów w C++](https://www.techiedelight.com/create-delete-directory-cpp/)
- [Poradnik dla początkujących - Tworzenie plików i katalogów w C++](https://www.learncpp.com/cpp-tutorial/creating-and-deleting-directories/)