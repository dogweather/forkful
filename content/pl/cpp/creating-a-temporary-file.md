---
title:    "C++: Tworzenie pliku tymczasowego"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego jest niezbędnym aspektem programowania w języku C++. Jest to bardzo przydatne narzędzie, które pozwala na przechowywanie danych tymczasowo i wykorzystanie ich w celu wykonania określonych funkcji. Przykładowo, plik tymczasowy może być używany do przechowywania ważnych informacji lub jako tymczasowe rozwiązanie w trakcie pracy nad kodem.

## Jak to zrobić

Aby utworzyć plik tymczasowy w języku C++, należy skorzystać z funkcji biblioteki standardowej "tmpnam()". Poniżej możesz zobaczyć przykładowy kod wykorzystujący tę funkcję:

```C++
#include <iostream>
#include <cstdio>

int main(){

    //Utworzenie wskaźnika na plik
    FILE* tempFile;

    //Używanie funkcji tmpnam() do utworzenia nazwy pliku tymczasowego
    char* tempFileName = tmpnam(NULL);

    //Otwieranie pliku w trybie do zapisu
    tempFile = fopen(tempFileName, "w");

    //Sprawdzenie, czy plik został poprawnie otwarty
    if (tempFile == NULL){
        std::cout << "Nie udało się utworzyć pliku tymczasowego." << std::endl;
        return 1;
    }

    //Zapisywanie pewnych danych do pliku
    fputs("To jest tekst tymczasowy", tempFile);

    //Zamykanie pliku
    fclose(tempFile);

    //Wyświetlenie nazwy utworzonego pliku
    std::cout << "Utworzono plik tymczasowy o nazwie: " << tempFileName << std::endl;

    return 0;
}
```

Powyższy kod skompilowany i uruchomiony w konsoli wyświetli następujący wynik:

```
Utworzono plik tymczasowy o nazwie: C:\Users\Username\AppData\Local\Temp\TMP000018624
```

Widzimy, że funkcja "tmpnam()" automatycznie generuje unikalną nazwę dla pliku tymczasowego, co jest bardzo przydatne. Możemy również zauważyć, że plik zostanie automatycznie usunięty po zakończeniu działania programu.

## Głębsza analiza

Funkcja "tmpnam()" działa poprzez tworzenie pliku w lokalizacji określonej przez system operacyjny jako katalog dla plików tymczasowych. Jest to bardzo wygodne rozwiązanie, ponieważ nie musimy martwić się o wybór odpowiedniego miejsca do przechowywania tymczasowych plików.

Należy jednak pamiętać, że funkcja ta nie jest bezpieczna w środowisku wielowątkowym, ponieważ może spowodować zgubienie danych lub utworzenie wielu plików o tej samej nazwie. W takim przypadku lepiej użyć funkcji "tmpfile()", która gwarantuje unikalność nazwy pliku tymczasowego.

## Zobacz też

- [Dokumentacja funkcji tmpnam() w standardzie C++](http://www.cplusplus.com/reference/cstdio/tmpnam/)
- [Przykładowe użycie funkcji tmpnam() na stronie cppreference.com](https://en.cppreference.com/w/cpp/io/c/tmpnam)