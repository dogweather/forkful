---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat to sposób sprawdzenia, która data jest wcześniejsza, późniejsza lub czy są takie same. Programiści robią to, aby umożliwić aplikacjom przetwarzanie danych w odpowiedni sposób w zależności od wyniku tej porównania.

## Jak to zrobić:

Metoda porównania dwóch dat w języku C polega na użyciu struktury `tm` oraz funkcji `mktime()`, `difftime()` i `localtime()`. Oto przykładowy kod:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // inicjalizujemy dwie struktury tm
    struct tm a = {0,0,0,2,1,112};   // 2 Luty, 2012
    struct tm b = {0,0,0,1,2,113};   // 1 Marzec, 2013

    // konwersja czasu do typu time_t
    time_t x = mktime(&a);
    time_t y = mktime(&b);

    // porównujemy dwie daty
    if (difftime(y, x) < 0)
        printf("Data a jest późniejsza od daty b\n");
    else
        printf("Data b jest późniejsza od daty a\n");

    return 0;
} 
```
## Dogłębna analiza

Porównanie dat w C nie jest tak proste jak w niektórych innych językach, które mają wbudowane daty i funkcje porównawcze. To jednak nie oznacza, że jest to trudne, wymaga tylko zrozumienia kilku funkcji i struktur języka C.

Alternatywą do powyższego kodu mogłoby być sparsowanie dwóch ciągów dat do struktury `tm` przy użyciu `strptime()`, a następnie porównanie ich jak powyżej.

Szczegółowo, funkcja `mktime()` konwertuje strukturę `tm` na `time_t`, `difftime()` porównuje dwa momenty w formacie `time_t`, a `localtime()` konwertuje `time_t` z powrotem na `tm`, jeśli potrzebujesz daty i czasu w czytelnej formie.

## Zobacz także

Spójrz na te linki dla więcej informacji:
- Dokumentacja dla [mktime()](https://www.cplusplus.com/reference/ctime/mktime/)
- Dokumentacja dla [difftime()](https://www.cplusplus.com/reference/ctime/difftime/)
- Więcej informacji o strukturze [tm](https://www.cplusplus.com/reference/ctime/tm/).