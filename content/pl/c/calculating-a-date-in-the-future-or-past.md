---
title:                "C: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego

C jest jednym z najpopularniejszych i najpotężniejszych języków programowania w dzisiejszych czasach. Jego uniwersalność i wydajność pozwala na tworzenie różnorodnych programów, w tym również narzędzi do obliczania dat w przyszłości lub przeszłości. Wydaje się to proste, ale jest to jednak bardzo przydatna i ważna funkcja, szczególnie dla programistów zajmujących się systemami czasu rzeczywistego.

# Jak to zrobić

Pisanie programów w języku C może wydawać się skomplikowane, ale zastosowanie odpowiednich bibliotek i funkcji może znacznie ułatwić zadanie. Najpierw należy zadeklarować zmienne, do których zostaną zapisane wyniki obliczeń. Następnie wewnątrz pętli for, przy użyciu funkcji z biblioteki time.h, obliczamy datę w przyszłości lub przeszłości. Poniższy kod jest przykładem obliczenia daty z 30 dniemdla kalendarza gregoriańskiego:

```C
#include<stdio.h>
#include<time.h>

int main()
{
    int dzien, miesiac, rok;
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    for(int i = 0; i < 30; i++)
    {
        dzien = tm.tm_mday + i;
        miesiac = tm.tm_mon + 1; //tm_mon zwraca miesiąc od 0, dlatego dodajemy 1
        rok = tm.tm_year + 1900;
        printf("Data za %d dni: %02d/%02d/%d.\n", i+1, dzien, miesiac, rok);
    }
    return 0;
}
```

Powyższy kod wykorzystuje funkcję time.h do pobrania aktualnego czasu i zapisania go do zmiennej t. Następnie funkcja localtime(&t) przepisuje czas do struktury tm, co pozwala na łatwiejsze manipulowanie datami i godzinami. W pętli for, za pomocą zwykłych działań matematycznych, obliczamy daty w przyszłości lub przeszłości i wyświetlamy je na ekranie. Pamiętajmy, że wartości w strukturze tm są indeksowane od 0, więc musimy dodać lub odjąć 1, aby uzyskać odpowiednią wartość dnia i miesiąca.

# Głębsza analiza

Obliczanie daty w przyszłości lub przeszłości może być potrzebne w różnych przypadkach. Na przykład, może być to przydatne w systemach rezerwacji, gdzie trzeba zarezerwować jakiś termin w przyszłości lub w symulacjach finansowych, gdzie trzeba przewidzieć dalsze wydarzenia. Kluczem do obliczenia daty w C jest funkcja time.h, która jest częścią standardowej biblioteki języka C i udostępnia wiele funkcji związanych z czasem i datą.

# Zobacz także

- [Documentacja funkcji time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Tutorial o manipulowaniu datami w C](https://www.programiz.com/c-programming/c-date-time)
- [Inne przydatne funkcje w bibliotece time.h](https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/)