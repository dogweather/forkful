---
title:    "C: Sprawdzanie istnienia katalogu"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak sprawdzić, czy dany katalog istnieje w systemie plików w języku C? W tym artykule dowiesz się, dlaczego jest to ważne i jak to zrobić. 

## Jak To Zrobić

Aby sprawdzić, czy dany katalog istnieje w systemie plików w języku C, musimy użyć funkcji "opendir" i "closedir" z biblioteki <dirent.h>. Skorzystajmy z poniższego kodu jako przykład:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
   char* path = "/home/user/Documents"; // ścieżka do sprawdzenia
   DIR* dir = opendir(path);
   
   if (dir) // jeśli funkcja "opendir" zwróciła niepuste wartości, oznacza to, że katalog istnieje
      printf("%s - Katalog istnieje!\n", path);
   else // w przeciwnym razie, katalog nie istnieje lub nie mamy do niego dostępu
      printf("%s - Katalog nie istnieje lub mamy brak dostępu.\n", path);
   
   closedir(dir); // zamknięcie katalogu
   return 0;
}
```

Wywołując powyższy kod, otrzymamy następujący wynik:

```
/home/user/Documents - Katalog istnieje!
```

W przypadku, gdy katalog nie istnieje lub nie mamy do niego dostępu, otrzymamy informację o tym fakcie.

## Deep Dive

Sprawdzenie, czy katalog istnieje, jest niezbędne w przypadku, gdy nasz program musi korzystać z plików znajdujących się w określonym miejscu. Bez uprzedniego sprawdzenia, czy katalog istnieje, nasz program może napotkać błąd i nie będzie w stanie działać poprawnie. 

Funkcja "opendir" zwraca wskaźnik do danego katalogu, co oznacza, że w przypadku sukcesu, możemy bez problemu odczytywać pliki znajdujące się w danym katalogu. W przypadku braku dostępu do katalogu lub gdy katalog nie istnieje, funkcja ta zwraca pusty wskaźnik, co umożliwia nam w porę wykryć potencjalny problem.

## Zobacz także
- [Funkcja opendir w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [Dokumentacja biblioteki dirent.h](http://pubs.opengroup.org/onlinepubs/007908799/xsh/dirent.h.html)
- [Przykłady użycia funkcji opendir w praktyce](http://www.martinbroadhurst.com/list-the-contents-of-a-directory-in-c.html)