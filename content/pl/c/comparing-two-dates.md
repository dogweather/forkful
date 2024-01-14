---
title:    "C: Porównywanie dwóch dat"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest częstym zadaniem w wielu programach, szczególnie w tych związanych z zarządzaniem czasem. Dzięki porównaniu dat możemy ustalić, która z nich jest wcześniejsza lub późniejsza, co pozwala nam na wykonywanie różnych operacji, takich jak sortowanie dat czy sprawdzanie poprawności danych. W tym artykule dowiesz się, jak porównywać daty w języku C.

## Jak to zrobić

Poniżej znajdują się przykładowe funkcje w języku C, które umożliwiają porównywanie dwóch dat. Przedstawione zostały również przykładowe wyniki ich działania, aby lepiej zrozumieć działanie kodu. Należy pamiętać, że w celu poprawnego porównania dat, musimy dysponować obiektami typu `struct tm`, które reprezentują datę i czas.

```C
#include <stdio.h>
#include <time.h>

int main() {

    // Ustalenie dat do porównania
    struct tm date1 = { .tm_year=2021, tm_mon=5, .tm_mday=12 };
    struct tm date2 = { .tm_year=2021, tm_mon=5, .tm_mday=10 };

    // Porównanie dat
    if (difftime(mktime(&date1), mktime(&date2)) > 0) {
        printf("Date 1 is later than date 2");
    } else if (difftime(mktime(&date1), mktime(&date2)) < 0) {
        printf("Date 2 is later than date 1");
    } else {
        printf("Both dates are equal");
    }

    return 0;
}
```

**Output:**

```
Date 1 is later than date 2
```

W powyższym przykładzie wykorzystujemy funkcję `mktime()`, która zamienia strukturę `struct tm` na liczbę sekund od 1 stycznia 1970 roku (tzw. "epoch time"). Następnie używamy funkcji `difftime()`, która porównuje te dwie liczby i zwraca różnicę w sekundach. Jeśli wynik jest większy od zera, oznacza to że pierwsza data jest późniejsza, jeśli mniejszy - druga data jest późniejsza, a jeśli równy zero, to obie daty są takie same.

## Głębszy wywód

Porównywanie dat może być trudniejsze, jeśli chcemy porównać tylko część daty, na przykład sam dzień, bez uwzględniania miesiąca i roku. W takim przypadku musimy wykorzystać funkcje dostępne w bibliotece C `time.h`, takie jak `gmtime()` czy `localtime()`, które pozwalają na pobieranie konkretnych informacji o dacie i czasie z obiektu `struct tm`.

Możemy również porównywać daty pod kątem kalendarza, uwzględniając różnice między latami przestępnymi, dzięki wykorzystaniu funkcji `isleap()`.

## Zobacz również

- [Dokumentacja C dla biblioteki time.h](https://www.gnu.org/software/libc/manual/html_node/Time_002dRelated-Types.html)
- [Porównywanie dat w języku C++](https://thispointer.com/difference-between-two-dates-in-days-months-years-in-c/)
- [Podstawy czasu w języku C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)

Dziękujemy za przeczytanie naszego artykułu o porównywaniu dat w języku C. Mamy nadzieję, że teraz jesteś lepiej przygotowany do wykorzystania tej umiejętności w swoich projektach.