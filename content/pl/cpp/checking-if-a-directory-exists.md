---
title:    "C++: Sprawdzanie czy istnieje katalog"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś początkującym programistą C++, być może zastanawiasz się, dlaczego powinieneś sprawdzać, czy dany katalog istnieje. Sprawdzenie istnienia katalogu jest ważnym krokiem w wielu aplikacjach, zwłaszcza tych związanych z przetwarzaniem plików. Podczas tworzenia programów, często musimy mieć pewność, że katalog, w którym przechowujemy nasze pliki, istnieje, aby uniknąć błędów i utraty danych. W tym artykule pokażę Ci, jak w łatwy sposób sprawdzić, czy dany katalog istnieje w Twoim programie w języku C++.

## Jak to zrobić

Istnieje kilka metod, aby sprawdzić, czy dany katalog istnieje w języku C++. Jedną z nich jest korzystanie z funkcji `std::filesystem::exists()` z biblioteki standardowej C++ 17. Przykładowy kod wykorzystujący tę funkcję wygląda następująco:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::string path = "/home/user/Documents";
    
    if (std::filesystem::exists(path)) {
        std::cout << "Katalog istnieje!" << std::endl;
    }
    else {
        std::cout << "Katalog nie istnieje!" << std::endl;
    }
    
    return 0;
}
```

Po uruchomieniu powyższego programu, jeśli katalog "/home/user/Documents" istnieje, powinno zostać wyświetlone na ekranie "Katalog istnieje!". W przeciwnym wypadku, napis "Katalog nie istnieje!" zostanie wyświetlony. Tak proste to!

Inną metodą jest użycie funkcji `access()` z biblioteki `<unistd.h>`. Możesz jej użyć w następujący sposób:

```C++
#include <iostream>
#include <unistd.h>

int main() {
    std::string path = "/home/user/Documents";
    
    if (access(path.c_str(), F_OK) == 0) {
        std::cout << "Katalog istnieje!" << std::endl;
    }
    else {
        std::cout << "Katalog nie istnieje!" << std::endl;
    }
    
    return 0;
}
```

Podobnie jak w poprzednim przykładzie, jeśli katalog istnieje, zostanie wyświetlony na ekranie odpowiedni komunikat. W przeciwnym wypadku, zostanie wyświetlone "Katalog nie istnieje!".

## Deep Dive

Podczas używania funkcji `access()`, warto zwrócić uwagę, że w przypadku gdy nie jesteśmy pewni, czy mamy prawa dostępu do danego katalogu, powinniśmy przekazać do niej drugi argument - `F_OK`. Jest to stała określająca, że sprawdzamy jedynie istnienie danego pliku lub katalogu. W przypadku gdy chcemy dodatkowo sprawdzić uprawnienia do odczytu, pisania lub wykonywania, możemy użyć odpowiednich stałych: `R_OK`, `W_OK`, `X_OK` (odpowiednio - dostęp do odczytu, zapisu i uruchamiania). Jeśli mamy problem z uprawnieniami, funkcja `access()` zwróci wartość -1.

Biblioteka `<sys/stat.h>` również udostępnia nam wiele funkcji przydatnych w wykrywaniu istnienia katalogu. Jedną z nich jest `stat()`, dzięki której możemy uzyskać informacje o danym pliku lub katalogu, w tym jego rozmiar, uprawnienia czy czas ostatniej modyfikacji. Przykładowy kod wykorzystujący tę funkcję może wyglądać tak:

```C++
#include <iostream>
#include <sys/stat.h>

int main() {
    std::string path = "/home/user/Documents";
    struct stat