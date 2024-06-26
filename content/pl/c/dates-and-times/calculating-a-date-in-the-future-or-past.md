---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:55.605303-07:00
description: "Jak to zrobi\u0107: Chocia\u017C standardowa biblioteka j\u0119zyka\
  \ C nie zapewnia bezpo\u015Brednich funkcji do arytmetyki dat, mo\u017Cna manipulowa\u0107\
  \ datami za pomoc\u0105 biblioteki\u2026"
lastmod: '2024-03-13T22:44:35.901083-06:00'
model: gpt-4-0125-preview
summary: "Chocia\u017C standardowa biblioteka j\u0119zyka C nie zapewnia bezpo\u015B\
  rednich funkcji do arytmetyki dat, mo\u017Cna manipulowa\u0107 datami za pomoc\u0105\
  \ biblioteki `time.h`, konkretnie pracuj\u0105c z typem danych `time_t` i struktur\u0105\
  \ `struct tm`."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Jak to zrobić:
Chociaż standardowa biblioteka języka C nie zapewnia bezpośrednich funkcji do arytmetyki dat, można manipulować datami za pomocą biblioteki `time.h`, konkretnie pracując z typem danych `time_t` i strukturą `struct tm`. Oto uproszczony przykład, jak dodać dni do bieżącej daty:

```c
#include <stdio.h>
#include <time.h>

void addDays(struct tm* date, int daysToAdd) {
    const time_t ONE_DAY = 24 * 60 * 60; // sekundy w jednym dniu
    // Konwersja struktury tm na time_t, dodanie dni i konwersja z powrotem
    time_t date_seconds = mktime(date) + (daysToAdd * ONE_DAY);
    *date = *localtime(&date_seconds);
}

int main() {
    time_t now;
    time(&now);
    struct tm futureDate = *localtime(&now);

    int daysToAdd = 10; // Dostosuj to do żądanej liczby dni do dodania
    addDays(&futureDate, daysToAdd);

    printf("Data w przyszłości: %d-%d-%d\n", futureDate.tm_year + 1900, futureDate.tm_mon + 1, futureDate.tm_mday);

    return 0;
}
```

Ten kod dodaje określoną liczbę dni do bieżącej daty i drukuje datę w przyszłości. Należy zauważyć, że podejście uwzględnia sekundy przestępne i korekty czasu letniego obsługiwane przez `mktime` i `localtime`.

Przykładowe wyjście:

```
Data w przyszłości: 2023-04-23
```

Należy pamiętać, że ten przykład dodaje dni, ale przy bardziej skomplikowanych obliczeniach (takich jak miesiące lub lata, biorąc pod uwagę lata przestępne), potrzebna byłaby bardziej zaawansowana logika lub biblioteki takie jak `date.h` w C++ lub biblioteki stron trzecich w C.

## Głębsze spojrzenie
Manipulacja datami w C za pomocą biblioteki time.h wiąże się z bezpośrednią manipulacją czasem w sekundach od epoki Uniksa (00:00, 1 stycznia 1970, UTC), a następnie przekształceniem tych sekund z powrotem na bardziej zrozumiały format daty (`struct tm`). To podejście jest prostym, ale skutecznym rozwiązaniem dla podstawowych operacji i korzyści z bycia międzyplatformowym oraz częścią standardowej biblioteki C.

Jednak simplicyzm tej metody jest również ograniczeniem. Zagadnienia związane z bardziej skomplikowanymi obliczeniami dat (takie jak uwzględnianie różnych długości miesięcy, lat przestępnych i stref czasowych) szybko stają się nietrywialne. Języki takie jak Python z `datetime` lub Java z `java.time` oferują bardziej intuicyjne API dla arytmetyki dat, przyjmując zasady programowania obiektowego dla większej przejrzystości i łatwości użycia.

W praktyce, pracując nad projektami wymagającymi obszernej manipulacji datami w C, programiści często zwracają się ku bibliotekom stron trzecich dla bardziej solidnych rozwiązań. Te biblioteki mogą oferować kompleksowe funkcje daty i czasu, w tym obsługę stref czasowych, opcje formatowania i bardziej subtelne możliwości arytmetyki dat, znacznie upraszczając zadanie programisty.

Pomimo dostępności bardziej nowoczesnych alternatyw, zrozumienie sposobu manipulowania datami za pomocą standardowej biblioteki C pozostaje cenną umiejętnością. Dostarcza ona głębokich wglądów w to, jak komputery reprezentują i pracują z czasem, fundamentalną koncepcję, która wykracza poza konkretne języki programowania.
