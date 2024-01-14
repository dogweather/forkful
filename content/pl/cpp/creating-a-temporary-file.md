---
title:                "C++: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest ważnym aspektem programowania, ponieważ pozwala nam dynamicznie tworzyć, przechowywać i usuwać pliki w naszym programie. Może to być przydatne w przypadku pracy z plikami tymczasowymi, tworzenia kopii zapasowych lub manipulowania danymi w naszej aplikacji.

## Jak to zrobić

Aby utworzyć tymczasowy plik w języku C++, musimy użyć funkcji biblioteki standardowej "tmpfile()" lub "tmpnam()". Funkcja "tmpfile()" tworzy i otwiera tymczasowy plik, zwracając wskaźnik do pliku, który możemy użyć do odczytu lub zapisu. Natomiast funkcja "tmpnam()" tworzy nazwę tymczasowego pliku i zwraca ją jako string. W obu przypadkach, plik zostanie automatycznie usunięty po zamknięciu programu.

```C++
#include <cstdio> //biblioteka zawierająca funkcje tmpfile() i tmpnam()
#include <iostream>
using namespace std;

int main() {
    //tworzenie tymczasowego pliku przy użyciu funkcji tmpfile()
    FILE* tempFile = tmpfile();
    if (tempFile != NULL) {
        //zapisanie tekstu do pliku
        fputs("To jest przykładowy tekst", tempFile);
        
        //sprawdzenie zawartości pliku
        cout << "Zawartość tymczasowego pliku: " << endl;
        rewind(tempFile); //ustawienie wskaźnika na początek pliku
        char c = fgetc(tempFile);
        while (c != EOF) { //czytanie pliku do końca
            cout << c;
            c = fgetc(tempFile);
        }
        fclose(tempFile); //zamknięcie pliku
    }
    
    //tworzenie nazwy tymczasowego pliku przy użyciu funkcji tmpnam()
    char* tempName = tmpnam(NULL); //NULL oznacza, że system wybierze miejsce na nazwę pliku
    if (tempName != NULL) {
        cout << "Nazwa tymczasowego pliku: " << tempName << endl;
    }
    
    return 0;
}
```

Przykładowy output:

```
Zawartość tymczasowego pliku:
To jest przykładowy tekst
Nazwa tymczasowego pliku: C:\Users\TEMPFI~1\AppData\Local\Temp/../1472.tmp
```

## Deep Dive

Istnieje wiele dodatkowych opcji, które możemy wykorzystać przy tworzeniu tymczasowego pliku. Na przykład, można ustawić prawa dostępu do pliku lub wybrać inny katalog, w którym zostanie utworzony. Możemy również wybrać, czy chcemy, aby plik był automatycznie usuwany po zamknięciu programu.

W przypadku funkcji "tmpfile()", możemy użyć funkcji "fopen()" do otwarcia tymczasowego pliku z wybranymi ustawieniami. Natomiast przy użyciu funkcji "tmpnam()", możemy wybrać pozycję, w której system będzie tworzył nazwę tymczasowego pliku, za pomocą funkcji "tempnam()".

Mimo że tworzenie tymczasowych plików jest bardzo przydatne, należy pamiętać, że nadal powinniśmy dbać o bezpieczeństwo naszych danych. W przypadku przechowywania poufnych informacji w tymczasowym pliku, należy upewnić się, że jest on odpowiednio usuwany i nie jest dostępny dla osób nieupoważnionych.

## Zobacz także

- [Funkcja tmpfile() w dokumentacji C++.com](https://en.cppreference.com/w/cpp/io/c/tmpfile)
- [Funkcja tmpnam() w dokumentacji C++.com](https://en.cppreference.com/w/cpp/io/c/tmpnam)
- [Artykuł o tworzeniu tymczasowych plików w C++ na blogu FluentCpp](https://www.fluentcpp.com