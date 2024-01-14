---
title:                "C: Uzyskiwanie aktualnej daty"
simple_title:         "Uzyskiwanie aktualnej daty"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w języku C to nie tylko nauka składni i struktury kodu, ale także umiejętność wykonywania codziennych zadań za pomocą kodu. Jednym z przydatnych zadań jest pobieranie aktualnej daty. W tym wpisie pokażemy, dlaczego jest to ważne i jak to zrobić w języku C.

## Jak to zrobić

Aby pobrać aktualną datę w języku C, użyjemy funkcji `time()` z biblioteki `time.h`. Następnie, możemy użyć struktury `tm` w bibliotece `time.h`, aby uzyskać dostęp do danych takich jak dzień, miesiąc, rok itp. W przykładzie poniżej wyświetlimy bieżącą datę w formacie DD-MM-RRRR.

```C
#include <stdio.h>
#include <time.h>

int main() {
   time_t czas;
   struct tm * data;
   char biezaca_data[100];
  
   czas = time(NULL);
   data = localtime(&czas);
  
   strftime(biezaca_data, sizeof(biezaca_data), "%d-%m-%Y", data);
  
   printf("Dzisiaj jest %s.\n", biezaca_data);

   return 0;
}
```

Przykładowy wynik:

```
Dzisiaj jest 20-05-2021.
```

## Głębsze zanurzenie

Funkcja `time()` zwraca liczbę sekund od 1 stycznia 1970 roku, znana jako czas epoki. Struktura `tm` zawiera pola takie jak `tm_sec`, `tm_min`, `tm_hour`, `tm_mday`, `tm_mon`, `tm_year` itp., które są używane do uzyskania danych o bieżącej dacie i godzinie. Więcej informacji na temat formatowania daty znajduje się w dokumentacji biblioteki `time.h.`

## Zobacz również

- [Dokumentacja biblioteki time.h w języku C](https://www.programiz.com/c-programming/library-function/time)
- [Przykłady użycia funkcji time()](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Inne przydatne funkcje czasu w języku C](https://fresh2refresh.com/c-programming/c-time-date-functions/)