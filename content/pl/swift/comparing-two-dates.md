---
title:                "Porównywanie dwóch dat"
html_title:           "Swift: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Co i dlaczego?
Porównywanie dwóch dat to proces polegający na porównaniu dwóch różnych dat w celu ustalenia, która z nich jest wcześniejsza lub późniejsza. Programiści często używają tego mechanizmu w celu wykrywania zmian w danych lub do śledzenia działań użytkowników.

# Jak to zrobić:
Sprawdzenie, czy jedna data jest wcześniejsza lub późniejsza od drugiej, można łatwo zrobić za pomocą kilku prostych linijek kodu w języku Swift. W poniższym przykładzie użyjemy funkcji ```compare``` z klasy ```Calendar```, która porównuje dwa obiekty typu ```Date``` i zwraca wartość typu ```ComparisonResult```:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let firstDate = dateFormatter.date(from: "01/01/2020")
let secondDate = dateFormatter.date(from: "05/01/2020")

let result = Calendar.current.compare(firstDate!, to: secondDate!, toGranularity: .day)

print(result)
```

W wyniku otrzymamy wartość ```ComparisonResult.orderedAscending```, co oznacza, że pierwsza data jest wcześniejsza od drugiej.

# Głębsze wgląd:
Porównywanie dat jest szczególnie przydatne w przypadkach, gdy program musi śledzić zmiany lub wyświetlać dane w kolejności chronologicznej. Alternatywą dla użycia funkcji ```compare``` jest użycie operatora ```<``` lub ```>```, który również porównuje dwie daty i zwraca wartość logiczną ```true``` lub ```false```. Proces porównywania dat jest również możliwy dzięki użyciu klas ```DateComponents``` i ```Calendar```, jednak wymaga to bardziej skomplikowanego kodu.

# Zobacz również:
Dla większej elastyczności w porównywaniu dat, warto zapoznać się również z biblioteką ```DateToolsSwift```, która oferuje wiele różnych funkcji związanych z operacjami na datach. Więcej informacji na ten temat możesz znaleźć na oficjalnej stronie projektu: https://github.com/MatthewYork/DateTools