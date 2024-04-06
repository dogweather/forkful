---
date: 2024-01-20 17:39:39.412613-07:00
description: "How to: (\"Dog\u0142\u0119bna analiza:\") W dawnych czasach programi\u015B\
  ci musieli r\u0119cznie zarz\u0105dza\u0107 tworzeniem i usuwaniem plik\xF3w tymczasowych.\
  \ C++17 wprowadzi\u0142o\u2026"
lastmod: '2024-04-05T22:50:50.068279-06:00'
model: gpt-4-1106-preview
summary: "(\"Dog\u0142\u0119bna analiza:\") W dawnych czasach programi\u015Bci musieli\
  \ r\u0119cznie zarz\u0105dza\u0107 tworzeniem i usuwaniem plik\xF3w tymczasowych."
title: Tworzenie pliku tymczasowego
weight: 21
---

## How to:
("Jak to zrobić:")
```C++
#include <iostream>
#include <filesystem>
#include <fstream>

int main() {
    // Stwórz unikalną ścieżkę pliku tymczasowego
    std::filesystem::path tempPath = std::filesystem::temp_directory_path() / "myapp_XXXXXX";

    // Utwórz i otwórz plik tymczasowy
    std::ofstream tempFile(tempPath);
    
    // Sprawdź, czy plik został pomyślnie utworzony
    if (tempFile.is_open()) {
        std::cout << "Plik tymczasowy został utworzony: " << tempPath << std::endl;
        
        // Użycie pliku...
        
        tempFile.close(); // Zamknij plik
    } else {
        std::cerr << "Nie udało się utworzyć pliku tymczasowego!" << std::endl;
    }
    
    // Usuń plik tymczasowy
    std::filesystem::remove(tempPath);
    return 0;
}
```
W powyższym przykładzie, stworzyliśmy plik tymczasowy w katalogu dla plików tymczasowych i wykorzystaliśmy go do jakiegoś zadania, a następnie go usunęliśmy.

## Deep Dive:
("Dogłębna analiza:")
W dawnych czasach programiści musieli ręcznie zarządzać tworzeniem i usuwaniem plików tymczasowych. C++17 wprowadziło `std::filesystem`, co uprościło zarządzanie plikami i folderami. Możemy wykorzystać `std::filesystem::temp_directory_path()` do znalezienia bezpiecznego miejsca dla plików tymczasowych różnych systemów operacyjnych, co czyni nasz kod przenośnym. Inne sposoby to m.in. użycie `tmpfile()` dla C lub `mkstemp()` dla systemów POSIX, które również tworzą i otwierają plik tymczasowy, ale są mniej elastyczne niż podejście C++17 z `std::filesystem`.

## See Also:
("Zobacz również:")
- Dokumentacja `std::filesystem`: https://en.cppreference.com/w/cpp/filesystem
- Porównanie `std::filesystem` i C POSIX functions: https://arne-mertz.de/2017/03/filesystem-convenience-posix/
- Wykorzystanie `std::tmpfile`: https://www.cplusplus.com/reference/cstdio/tmpfile/
