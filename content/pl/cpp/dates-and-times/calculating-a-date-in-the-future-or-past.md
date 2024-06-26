---
date: 2024-01-20 17:28:33.161906-07:00
description: "Jak to zrobi\u0107? W C++, obliczanie daty w przesz\u0142o\u015Bci lub\
  \ przysz\u0142o\u015Bci jest \u0142atwiejsze z bibliotek\u0105 `<chrono>`, wprowadzon\u0105\
  \ w C++11. Wcze\u015Bniej, programi\u015Bci\u2026"
lastmod: '2024-04-05T22:50:50.061906-06:00'
model: gpt-4-1106-preview
summary: "W C++, obliczanie daty w przesz\u0142o\u015Bci lub przysz\u0142o\u015Bci\
  \ jest \u0142atwiejsze z bibliotek\u0105 `<chrono>`, wprowadzon\u0105 w C++11."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Jak to zrobić?
```c++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    using namespace std::chrono;
    
    // Pobieramy aktualny czas systemowy i przekształcamy na czas lokalny
    system_clock::time_point today = system_clock::now();
    time_t tt = system_clock::to_time_t(today);
    tm local_tm = *localtime(&tt);

    // Dodajemy 30 dni do aktualnej daty
    system_clock::time_point future_date = today + days(30);
    tt = system_clock::to_time_t(future_date);
    tm future_tm = *localtime(&tt);

    // Formatujemy i wyświetlamy datę
    std::cout << "Dzisiejsza data to: " << std::put_time(&local_tm, "%d-%m-%Y") << '\n';
    std::cout << "Data za 30 dni to: " << std::put_time(&future_tm, "%d-%m-%Y") << '\n';

    return 0;
}
```
Sample Output:
```
Dzisiejsza data to: 15-04-2023
Data za 30 dni to: 15-05-2023
```

## Warto Wiedzieć
W C++, obliczanie daty w przeszłości lub przyszłości jest łatwiejsze z biblioteką `<chrono>`, wprowadzoną w C++11. Wcześniej, programiści musieli polegać na skomplikowanej matematyce i funkcjach z rodziny C `time.h`. Biblioteka `<chrono>` oferuje nowocześniejsze i bezpieczniejsze podejście.

Alternatywy? Możesz użyć bibliotek zewnętrznych jak Boost lub Qt, które mają własne mechanizmy dat i czasu. Jednak `<chrono>` jest częścią standardowej biblioteki, co oznacza mniej zależności i (zazwyczaj) łatwiejsze utrzymanie.

Szczegóły implementacyjne? Gdy pracujesz z datami, pamiętaj o strefach czasowych i przestępnych sekundach. Złożoność czasu jest potężna, ale C++ pomaga w jej uproszczeniu przy pomocy typów takich jak `system_clock::time_point` czy `duration`.

## Zobacz Również
- Dokumentacja C++ `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- Tutorial Boost DateTime: https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html
- Tutorial Qt QDateTime: https://doc.qt.io/qt-5/qdatetime.html
