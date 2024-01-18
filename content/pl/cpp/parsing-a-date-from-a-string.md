---
title:                "Przetwarzanie daty z ciągu znaków"
html_title:           "C++: Przetwarzanie daty z ciągu znaków"
simple_title:         "Przetwarzanie daty z ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Czym jest parsowanie daty z ciągu znaków i dlaczego programiści to robią?

Parsowanie daty z ciągu znaków jest procesem konwertowania daty w postaci tekstowej na format, który może być używany przez program komputerowy. Programiści często wykonują ten proces, aby dostosować dane wejściowe, które otrzymują z użytkownika lub innych źródeł, do odpowiedniego formatu, który można wykorzystać w programie.

## Jak to zrobić?

```C++
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>

int main() {
  std::string date = "2021/05/25";
  std::stringstream ss(date);

  int year, month, day;
  char delimiter; // separator daty

  ss >> year >> delimiter >> month >> delimiter >> day;

  std::cout << "Rok: " << year << std::endl;
  std::cout << "Miesiąc: " << month << std::endl;
  std::cout << "Dzień: " << day << std::endl;

  return 0;
}
```

**Wynik:**

```
Rok: 2021
Miesiąc: 05
Dzień: 25
```

## Głębsze zagadnienia

### Kontekst historyczny

Parsowanie daty z ciągu znaków jest ważną częścią programowania od początku istnienia języków programowania. Dawniej, kiedy dysponowano tylko ograniczoną ilością pamięci i zasobów, parsowanie daty było uważane za bardzo trudne zadanie. Dziś, dzięki nowoczesnym językom programowania i bibliotekom, jest to znacznie prostsze.

### Alternatywy

Istnieje wiele alternatywnych metod parsowania daty z ciągu znaków, a niektóre z nich są dostępne w różnych językach programowania. Jedną z najpopularniejszych jest wykorzystanie biblioteki `datetime` w języku Python, która oferuje wbudowane funkcje do przetwarzania daty i czasu.

### Szczegóły implementacji

Istnieje wiele sposobów implementacji parsowania daty z ciągu znaków w języku C++. W przykładzie powyżej wykorzystano strumień `stringstream`, ale można również użyć biblioteki `boost::date_time` lub funkcji pomocniczych `strptime` lub `sscanf` z biblioteki standardowej.

## Zobacz też

- [Dokumentacja biblioteki datetime w języku Python](https://docs.python.org/3/library/datetime.html)
- [Dokumentacja biblioteki boost::date_time w języku C++](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)
- [Strona projektu boost::date_time](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time/manual.html)