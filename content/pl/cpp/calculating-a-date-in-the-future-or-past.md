---
title:                "C++: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego
Obliczanie daty w przyszłości lub przeszłości może być bardzo pomocne w wielu programach i aplikacjach. Może to być wykorzystane na przykład do tworzenia planów wydarzeń lub przewidywania określonych dat.

## Jak to zrobić
Obliczanie daty przyszłej lub przeszłej w języku C++ jest niezwykle proste. Wystarczy wykorzystać wbudowane funkcje w bibliotece <ctime>, takie jak mktime() i localtime(). Następnie można użyć ich w kombinacji z niestandardowymi funkcjami, aby obliczyć odpowiednią datę.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
	// pobranie aktualnej daty i godziny
	time_t t = time(NULL);
  	struct tm *now = localtime(&t);

    // obliczenie daty w przyszłości (tu: 7 dni od dzisiaj)
  	now->tm_mday += 7;
  	mktime(now);

    // wyświetlenie obliczonej daty
  	cout << "Data w przyszłości: " << (now->tm_year + 1900) << "-" << (now->tm_mon + 1) << "-" << now->tm_mday << endl;

    // obliczenie daty w przeszłości (tu: 30 dni przed dzisiaj)
  	now->tm_mday -= 30;
  	mktime(now);

    // wyświetlenie obliczonej daty
  	cout << "Data w przeszłości: " << (now->tm_year + 1900) << "-" << (now->tm_mon + 1) << "-" << now->tm_mday << endl;

  return 0;
}
```

Output:
```
Data w przyszłości: 2020-6-24
Data w przeszłości: 2020-4-25
```

## Głębsze zagadnienia
Obliczanie daty w przyszłości lub przeszłości może wymagać bardziej złożonych obliczeń, takich jak uwzględnienie różnego czasu w różnych strefach czasowych czy błędów wynikających z roku przestępnego. Dlatego warto zwrócić uwagę na szczegóły implementacji i przetestować różne scenariusze, aby upewnić się o poprawności wyniku.

## Zobacz także
- Dokumentacja biblioteki <ctime>: http://www.cplusplus.com/reference/ctime/
- Przewodnik po pracy z datami w C++: https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm
- Przykładowe kody obliczające datę w C++: https://www.geeksforgeeks.org/date-functions-in-c-c/