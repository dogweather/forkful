---
title:                "Konwersja daty na łańcuch znaków"
date:                  2024-01-20T17:36:02.997106-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Konwersja daty na ciąg znaków (string) to proces przekształcenia formatu daty na tekst, który łatwo przeczytamy lub wyświetlimy. Programiści robią to, by data była zrozumiała dla użytkowników i mogła być zapisana w logach lub dokumentach.

## Jak to zrobić:
W C++ mamy kilka sposobów na przekształcenie daty w string - najprościej użyć biblioteki `<chrono>` i `<iomanip>`:

```C++
#include <iostream>
#include <iomanip>
#include <chrono>
#include <sstream>

int main() {
    // Pobranie aktualnego czasu
    auto teraz = std::chrono::system_clock::now();
    
    // Konwersja czasu na format tm
    std::time_t czasC = std::chrono::system_clock::to_time_t(teraz);
    std::tm* czasTM = std::localtime(&czasC);

    // Utworzenie stringa z datą
    std::stringstream ss;
    ss << std::put_time(czasTM, "%Y-%m-%d %H:%M:%S");

    // Wyświetlenie przekonwertowanej daty
    std::string dataJakoString = ss.str();
    std::cout << "Aktualna data i czas jako string: " << dataJakoString << std::endl;
    
    return 0;
}
```

Sample output:
```
Aktualna data i czas jako string: 2023-04-05 15:30:21
```

## W głąb tematu:
Data i czas w C++ ewoluowały. Początkowo korzystano z funkcji C jak `ctime()` czy `strftime()`. Pojawił się nawet `boost::date_time`, ale od C++11 mamy `<chrono>` i to stało się standardem.

Alternatywy:
- Biblioteki zewnętrzne jak `boost::date_time`
- Staromodne funkcje C

Szczegóły implementacyjne:
W `<chrono>`, `system_clock::now()` zapewnia czas bazujący na zegarze systemowym. `.to_time_t()` konwertuje czas na tradycyjny format C, który można następnie użyć z funkcjami formatującymi czas sprzed C++11, takimi jak `std::put_time()`.

`std::stringstream` i `std::put_time` pozwalają na elastyczne formatowanie daty i czasu jako string.

## Zobacz też:
- Dokumentacja C++ std::chrono: https://en.cppreference.com/w/cpp/chrono
- Biblioteka Boost.Date_Time: https://www.boost.org/doc/libs/release/libs/date_time/
- Dokumentacja C strftime: https://en.cppreference.com/w/cpp/chrono/c/strftime
