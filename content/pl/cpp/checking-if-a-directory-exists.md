---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "C++: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzanie, czy katalog istnieje, to prosta operacja, która polega na potwierdzaniu, czy dany katalog rzeczywiście istnieje w systemie plików. Programiści robią to po to, aby uniknąć błędów podczas próby otwarcia katalogu, który nie istnieje.

## Jak to zrobić:

Użyjemy tutaj funkcji 'exists()' z biblioteki filesystem. Tutaj jest przykładowy kod:

```C++
#include <filesystem>

bool doesDirExist(const std::string& dirPath) {
    return std::filesystem::exists(dirPath);
}

int main() {
    std::string dirToCheck = "/path/to/directory";
   
    if (doesDirExist(dirToCheck)) {
        std::cout << "Katalog istnieje.\n";
    } else {
        std::cout << "Katalog nie istnieje.\n";
    }
    
    return 0;
}
```
Gdy skompilujemy i uruchomimy powyższy program, dostaniemy dane wyjściowe w zależności od tego, czy sprawdzany katalog istnieje czy nie.

## Dogłębna analiza

1. Kontekst historyczny: W przeszłości musieliśmy polegać na specyficznych dla platformy wywołaniach API, aby sprawdzić, czy katalog istnieje. Ale od C++17 mamy do dyspozycji przenośne narzędzia w bibliotece filesystem.

2. Alternatywy: Można również używać funkcji `opendir()` dla Unix/Linux lub `GetFileAttributes()` dla Windows. Ale te metody są specyficzne dla platform, co czyni je mniej przenośnymi.

3. Szczegóły implementacji: `std::filesystem::exists()` sprawdza, czy dana ścieżka istnieje w systemie plików. Sprawdza ona nie tylko katalogi, ale również pliki, co czyni ją bardziej uniwersalną.

## Zobacz także:

1. Dokumentacja C++ `std::filesystem::exists()`: 
https://en.cppreference.com/w/cpp/filesystem/exists

2. Poradnik C++17 o bibliotece filesystem:
https://www.learncpp.com/cpp-tutorial/an-introduction-to-stdfilesystem/