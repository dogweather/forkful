---
title:    "C++: Sprawdzanie, czy istnieje katalog"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy katalog istnieje, jest ważną częścią pisania oprogramowania w języku C++. Pomaga to upewnić się, że program będzie działać poprawnie, a także uniknąć błędów.

## Jak to zrobić

Sprawdzenie, czy katalog istnieje, jest stosunkowo proste w języku C++. Aby to zrobić, użyjemy funkcji `opendir` z biblioteki `dirent.h`. Następnie wykonamy pętlę, aby przejść przez wszystkie pliki w katalogu i sprawdzić, czy któryś z nich jest katalogiem.

```C++
#include <dirent.h>
#include <string>
#include <iostream>

using namespace std;

int main() {
    // Utworzenie wskaźnika na katalog
    DIR *dir;
    // Utworzenie struktury przechowującej informacje o pliku w katalogu
    struct dirent *entry;

    // Nazwa katalogu do sprawdzenia
    string directory_name = "testowy_katalog";

    // Otwarcie katalogu
    dir = opendir(directory_name.c_str());
    
    // Sprawdzenie, czy udało się otworzyć katalog
    if (dir == nullptr) {
        cerr << "Nie można otworzyć katalogu!";
        return -1;
    }

    // Pętla przez wszystkie pliki w katalogu
    while ((entry = readdir(dir)) != nullptr) {
        // Jeśli jest to katalog, wyświetl informację
        if (entry->d_type == DT_DIR) {
            cout << "Katalog " << entry->d_name << " istnieje!" << endl;
        }
    }

    // Zamknięcie katalogu
    closedir(dir);
    
    return 0;
}
```

### Przykładowy wynik:

```
Katalog . istnieje!
Katalog .. istnieje!
Katalog podkatalog istnieje!
```

## Vertigo jako pomoc

Vertigo jest narzędziem, które może znacznie ułatwić pracę z plikami i katalogami w języku C++. Jest to open-source'owy projekt, który zawiera wiele przydatnych funkcji, w tym również funkcję `isDirectory` do sprawdzania, czy podana ścieżka jest katalogiem.

Jeśli zdecydujesz się skorzystać z Vertigo, aby sprawdzić, czy katalog istnieje, kod będzie wyglądał następująco:

```C++
#include "vertigo.hpp"
#include <iostream>

using namespace std;
using namespace Vt;

int main() {
    // Nazwa katalogu do sprawdzenia
    string directory_name = "testowy_katalog";
    
    // Wykorzystanie funkcji isDirectory z Vertigo
    if (isDirectory(directory_name)) {
        cout << "Katalog istnieje!" << endl;
    } else {
        cout << "Katalog nie istnieje!" << endl;
    }
    
    return 0;
}
```

## Zanurzenie się

Dodatkową informacją, którą warto wiedzieć, jest fakt, że funkcja `opendir` zwraca wskaźnik do pierwszego pliku w katalogu. Jeśli chcemy wylistować wszystkie pliki, a nie tylko katalogi, musimy użyć funkcji `lstat` z biblioteki `sys/types.h` wewnątrz pętli.

Zamiast `entry->d_type == DT_DIR` możemy użyć `lstat(entry->d_name, &buf) != 0 && S_ISREG(buf.st_mode)`, aby sprawdzić, czy plik jest zwykłym plikiem, a nie katalogiem.

Dzięki temu możemy uzyskać dostęp nie tylko do katalogów, ale również do innych plików wewnątrz badanego katalogu.

## Zobacz także

- [Dokumentacja funkcji opendir](http://man7.org/linux/man-pages/man3/opendir.3.html)
- [Dokumentacja funkcji readdir](http://man7.org/linux/man-pages/man3/readdir.3.html)
- [Dokumentacja funkcji lstat](http://man7.org/linux/man-pages/man