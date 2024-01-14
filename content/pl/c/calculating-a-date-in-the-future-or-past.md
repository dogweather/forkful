---
title:                "C: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Czy dlaczego warto obliczyć datę w przyszłości lub przeszłości?

Obliczanie daty w przyszłości lub przeszłości może być bardzo przydatne w wielu sytuacjach, na przykład w tworzeniu aplikacji internetowych obsługujących rezerwacje lub planowanie wydarzeń. Może również być używane do obliczania terminów płatności lub dat ważności. Dzięki temu możemy uniknąć pomyłek i zaoszczędzić czas na ręczne wyliczanie dat.

## Jak to zrobić?

W poniższym przykładzie pokażemy, jak napisanie jednej funkcji w C może pomóc nam w obliczaniu daty w przyszłości lub przeszłości. Zobaczmy kod w akcji:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// funkcja do obliczania daty w przyszłości lub przeszłości
// przyjmuje trzy argumenty: rok, miesiąc i dzień

void oblicz_date(int rok, int miesiac, int dzien) {
    // zdefiniuj strukturę tm do przechowywania daty
    struct tm data = { .tm_year=rok-1900, .tm_mon=miesiac-1, .tm_mday=dzien};

    // zdefiniuj zmienne sekundy i czasu
    time_t sekundy;
    struct tm *czas;

    // pobierz czas bieżący
    time(&sekundy);

    // oblicz datę w przyszłości lub przeszłości
    sekundy += mktime(&data);

    // skonwertuj czas na strukturę gotową do wyświetlenia
    czas = localtime(&sekundy);

    // wyświetl datę w czytelny sposób
    printf("Nowa data: %02d/%02d/%d\n", czas->tm_mday, czas->tm_mon+1, czas->tm_year+1900);
}

int main() {
    // przykładowe wywołanie funkcji
    oblicz_date(2020, 5, 15);
    return 0;
}
```
W tym przykładzie korzystamy z funkcji `mktime()` i `localtime()` z biblioteki `time.h` do obliczenia daty w przyszłości lub przeszłości, uwzględniając podane przez nas dane. Jeśli na przykład chcemy obliczyć datę, która jest 5 dni po podanej, wystarczy dodać 5 do `data.tm_mday`. 

## Głębszy zanurzenie

Obliczanie daty w przyszłości lub przeszłości może być bardziej skomplikowane, gdy uwzględnia się skoki czasowe lub różnice w datach między różnymi państwami. W takich przypadkach należy dokładnie zapoznać się z dokumentacją funkcji `mktime()` i pozostałych funkcji z biblioteki `time.h`, aby osiągnąć poprawne wyniki.

Inną użyteczną funkcją, która może pomóc w obliczaniu dat, jest `difftime()` z tej samej biblioteki. Pozwala ona na obliczenie różnicy w czasie między dwoma strukturami `tm`.

## Zobacz również
- [Dokumentacja funkcji mktime()](https://www.cplusplus.com/reference/ctime/mktime/)
- [Dokumentacja funkcji localtime()](https://www.cplusplus.com/reference/ctime/localtime/)
- [Poradnik dla początkujących w C](https://www.cprogramming.com/begin.html)