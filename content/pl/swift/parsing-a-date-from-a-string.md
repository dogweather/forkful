---
title:                "Wyodrębnianie daty z ciągu znaków."
html_title:           "Swift: Wyodrębnianie daty z ciągu znaków."
simple_title:         "Wyodrębnianie daty z ciągu znaków."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty z ciągu znaków to proces, w którym program przetwarza ciąg znaków w formacie daty na odpowiedni obiekt w swoim kodzie. Programiści wykonują tę czynność, aby umożliwić aplikacji interakcję z danymi datowymi, takimi jak daty urodzenia lub terminy spotkań.

## Jak to zrobić:

```Swift
let inputDate = "20-04-2021"

let formatter = DateFormatter()
formatter.dateFormat = "dd-MM-yyyy"

if let date = formatter.date(from: inputDate) {
    print(date) // 2021-04-20 00:00:00 +0000
}
```

## Wnikliwa analiza:

W kontekście programowania, parsowanie daty z ciągu znaków jest ważnym procesem, ponieważ umożliwia interakcję z różnymi formatami dat w aplikacji. Alternatywą dla parsowania daty jest wykorzystanie gotowych bibliotek, takich jak "DateUtils", jednak może to wymagać utrzymywania dodatkowych zależności w projekcie.

W powyższym przykładzie użyto klasy DateFormatter w języku Swift, która dostarcza funkcjonalności do przekształcania daty z jednego formatu na inny. W przypadku, gdy parsowanie zwraca wartość nil, oznacza to, że dana data w ciągu znaków jest nieprawidłowa i należy dostosować format podany w metodzie "dateFormat" do tej daty.

## Zobacz również:

Dowiedz się więcej o klasie DateFormatter w dokumentacji języka Swift – https://developer.apple.com/documentation/foundation/dateformatter

Szukaj gotowych rozwiązań bibliotek do parsowania dat w aplikacjach iOS – https://github.com/search?q=date+utils+ios