---
title:                "C: Pobieranie aktualnej daty"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Otrzymywanie aktualnej daty jest ważnym elementem wielu programów. Często musimy wiedzieć, jaka jest bieżąca data, aby wykonać pewne działania, takie jak generowanie raportów lub tworzenie datowników.

## Jak to zrobić

W języku C istnieje kilka sposobów na uzyskanie aktualnej daty. Jednym z nich jest użycie funkcji `time()`, która zwraca liczbę sekund, które upłynęły od 1 stycznia 1970 roku (tzw. Epoce Unixa). Aby przekazać tę liczbę na bieżącą datę, możemy użyć funkcji `localtime()`, która konwertuje tę liczbę na strukturę `tm` zawierającą informacje o dacie i czasie. Poniżej znajduje się przykładowy kod:

```C
#include <stdio.h>
#include <time.h>

int main(void)
{
    // uzyskanie liczby sekund od epoki Unix-a
    time_t current_time = time(NULL);

    // konwersja do struktury tm
    struct tm *t = localtime(&current_time);

    // wyświetlenie bieżącej daty
    printf("Bieżąca data: %d.%d.%d\n", t->tm_mday, t->tm_mon+1, t->tm_year+1900);

    return 0;
}
```

Wyjście z powyższego kodu może wyglądać następująco, w zależności od bieżącej daty:

```
Bieżąca data: 10.11.2021
```

## Głębsze zagadnienia

W przypadku uzyskiwania bieżącej daty mogą pojawić się pewne problemy związane z różnymi strefami czasowymi i zmianami czasu letniego. W takim przypadku warto sięgnąć po bardziej zaawansowane funkcje takie jak `gettimeofday()` lub `localtime_r()`.

Jednym z problemów związanych ze zmianą czasu letniego jest to, że w niektórych przypadkach funkcja `localtime()` może zwrócić niepoprawną datę, ponieważ pole `tm_isdst` w strukturze `tm` może być ustawione na wartość `1` lub `0` zamiast oczekiwanej `-1`. Aby tego uniknąć, warto użyć funkcji `localtime_r()`, która pozwala na wykorzystanie dodatkowego parametru umożliwiającego określenie, czy system używa czasu letniego czy nie.

## Zobacz także

- Oficjalna dokumentacja języka C: https://en.cppreference.com/w/c
- Praca z datami i czasem w języku C: https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm
- Problem ze zmianą czasu letniego w funkcji `localtime()`: http://man7.org/linux/man-pages/man3/localtime.3.html