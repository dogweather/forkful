---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "C: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Co i dlaczego? 

Spójrzmy prawdzie w oczy - jak programiści mamy wiele zadań do wykonania, a jednym z nich jest sprawdzanie, czy dany katalog istnieje. Może wydawać się to trywialne, ale w rzeczywistości jest to ważna część procesu programowania. Sprawdzanie istnienia katalogu jest niezbędne, aby zapewnić, że nasz program działa poprawnie i nie wywala się na błędzie. 

# Instrukcja:

Oto przykład kodu, który sprawdzi, czy dany katalog istnieje i wyświetli odpowiedni komunikat: 

```C
#include <stdio.h> 
#include <sys/stat.h> 

int main() { 
    struct stat st = {0}; 
    char* path = "/my/directory/path"; 

    // funkcja stat() pobiera informacje o pliku lub katalogu
    if (stat(path, &st) == 0) { 
        // jeśli wynik jest równy 0, oznacza to, że katalog istnieje 
        printf("Katalog %s istnieje.\n", path); 
    } 
    else { 
        // jeśli wynik jest inny niż 0, oznacza to, że katalog nie istnieje lub wystąpił błąd 
        printf("Katalog %s nie istnieje.\n", path); 
    } 

    return 0; 
} 
```

Przykładowy wynik: 

``` 
Katalog /my/directory/path istnieje. 
```


# Głębsza analiza:

Sprawdzanie, czy dany katalog istnieje, jest ważną częścią programowania od samego początku. Starsze języki programowania, takie jak C, nie posiadały wbudowanych funkcji do tego celu, dlatego trzeba było opracować różne sposoby na rozwiązanie tego problemu. Jedną z metod było wykorzystanie funkcji "stat", która pobiera informacje o pliku lub katalogu i zwraca wynik w postaci struktury. W przypadku istnienia katalogu, funkcja zwraca wartość 0, w przeciwnym razie inny wynik. Obecnie istnieją już standardowe funkcje, takie jak "opendir" czy "access", które ułatwiają sprawdzanie istnienia katalogu. 

# Zobacz również:

Dla bardziej zaawansowanych informacji na temat sprawdzania istnienia katalogu możesz zajrzeć na te strony: 

- [Dokumentacja funkcji stat() w języku C](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.2.0/com.ibm.zos.v2r2.bpxbd00/stat.htm) 
- [Porównanie funkcji stat(), opendir() i access()](https://stackoverflow.com/questions/21644669/what-is-the-difference-between-stat-opendir-access-in-unix-c-programming)