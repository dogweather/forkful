---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Analiza daty z ciągu tekstowego to proces przetwarzania tekstu, który reprezentuje datę i czas, do obiektu `Date` w Swift. Programiści robią to, aby poradzić sobie z różnymi formatami dat i czasu używanymi w różnych częściach świata, oraz w celu manipulowania i porównywania dat.

## Jak to zrobić:
```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"

let dateInString = "2022-11-15 18:00:00"
let date = dateFormatter.date(from: dateInString)

print(date)
```
Wynik:
```Swift 
Optional(2022-11-15 18:00:00 +0000)
```

## Zagłębiamy się:
**Kontekst historyczny**
Początki analizowania daty z ciągu są związane z pytaniami o ilość dni między datami, daty przyszłego wydarzenia itp, które są trudne do przeliczenia ręcznie. 

**Alternatywy**
Inne metody analizowania daty z ciągu to na przykład użycie `ISO8601DateFormatter` w Swift dla ciągów w formacie ISO 8601. 

**Szczegóły implementacji**
Podczas analizy daty, używany jest określony format daty, który musi pasować do formatu ciągu, który próbujesz przetworzyć. W przeciwnym razie zwraca `nil`. 

## Zobacz też:
1. Dokumentacja Apple o [analizowaniu dat](https://developer.apple.com/documentation/foundation/dateformatter#2843288).
2. Post na StackOverflow na temat [różnych formatów dat](https://stackoverflow.com/questions/35700281/date-format-in-swift).
3. Więcej informacji o [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html).