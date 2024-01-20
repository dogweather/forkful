---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie aktualnej daty w programowaniu to proces wyznaczania bieżącej daty i godziny. Programiści często korzystają z tej funkcji do znacznikowania danych, monitorowania pamięci podręcznej lub ustalania interwałów czasowych.

## Jak to zrobić:

Rozważmy proces za pomocą kodu Swift. Dla uzyskania aktualnej daty, używamy klasa `Date()`.

```Swift
let AktualnaData = Date()
print(AktualnaData)
```

W przypadku uruchomienia tego kodu uzyskalibyśmy wydruk bieżącej daty i czasu, np.:

```Swift
2022-10-01 17:50:53 +0000
```

## Pogłębiona analiza

Pobieranie aktualnej daty to stara praktyka w programowaniu, stosowana na przykład w systemach operacyjnych UNIX już w latach 70. XX wieku. W Swift istnieją również alternatywne metody pobierania daty, takie jak korzystanie z `DateComponents()` lub `NSDate()`.

Szczegóły implementacji `Date()` polegają na wykorzystaniu czasu UTC. Oznacza to, że data i godzina są zawsze przedstawiane w odniesieniu do czasu uniwersalnego, a nie lokalnego czasu użytkownika. Aby przeliczyć na czas lokalny, potrzebny jest obiekt `TimeZone`.

## Zobacz też 

1. Dokumentacja Swift `Date`: https://developer.apple.com/documentation/foundation/date
2. Dokumentacja Swift `DateComponents`: https://developer.apple.com/documentation/foundation/datecomponents
3. Przewodnik po czasie i dacie w Swift: https://www.hackingwithswift.com/articles/141/8-practical-time-and-date-handling-with-swift