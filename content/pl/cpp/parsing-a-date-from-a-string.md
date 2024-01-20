---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

"## Co i Dlaczego?"
Analiza daty z ciągu to proces przekształcania tekstu w wartość daty. Dzięki temu programiści mogą łatwo manipulować danymi związanymi z datą i czasem oraz porównywać różne formaty dat.

"## Jak to zrobić:"
Oto, jak w C++ możemy przekształcić tekst w datę.

```C++
#include <string>
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
  std::string data_string = "2002-09-24";
  std::istringstream ss(data_string);
  std::tm tm = {};

  ss >> std::get_time(&tm, "%Y-%m-%d");

  if (ss.fail()) {
    std::cout << "Nie udało się przeparsować daty.\n";
  } else {
    std::cout << std::put_time(&tm, "%Y-%m-%d") << '\n';
  }

  return 0;
}
```

Przykładowe wyjście:

```
2002-09-24
```

"## Deep Dive":
Przy analizowaniu daty z ciągu, ważne jest zrozumienie kontekstu historycznego. W latach 70. i 80., podczas początkowych dziesięcioleci informatyki, analiza daty była rzadko stosowanym zadaniem, a formaty dat były różne w zależności od lokalnych norm i praktyk. W miarę upowszechniania się sieci komputerowych i potrzeby interoperacyjności, pojawiła się potrzeba standardizacji.

Jednym z alternatywnych podejść jest użycie bibliotek zewnętrznych, takich jak Boost.Date_time, które zapewniają potężne i elastyczne narzędzia do przetwarzania daty i czasu.

Co do szczegółów implementacji, funkcja `std::get_time` jest templatką, która wyodrębnia wartości czasu z ciągu znaków zgodnie z formatem podanym jako drugi argument, a następnie umieszcza wyniki w strukturze `std::tm`.

"## Zobacz też":
1. Dokumentacja C++ - Date and Time utilities: https://en.cppreference.com/w/cpp/chrono
2. Strona Boost.Date_Time: https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html