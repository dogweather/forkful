---
title:                "Pobieranie aktualnej daty"
html_title:           "Swift: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego chcielibyśmy uzyskać aktualną datę w naszym kodzie Swift? Powodów może być wiele - od wyświetlania aktualnego czasu w naszej aplikacji po porównywanie dat między różnymi obiektami. Bez względu na cel, uzyskanie bieżącej daty jest ważną częścią wielu projektów.

## Jak to zrobić

Często chcemy uzyskać aktualną datę w naszym kodzie, a Swift udostępnia nam prosty sposób na to - używając typu `Date`. Możemy użyć funkcji `Date()` aby utworzyć obiekt daty, a następnie użyć wybranych metod, aby uzyskać informacje, które nas interesują.

Przykład kodu:

```Swift
let currentDate = Date()

let calendar = Calendar.current
let year = calendar.component(.year, from: currentDate)
let month = calendar.component(.month, from: currentDate)
let day = calendar.component(.day, from: currentDate)

print("\(day)/\(month)/\(year)")
```

Wyjście powyższego kodu będzie zależeć od aktualnej daty, ale może wyglądać tak:

```
26/3/2021
```

W ten sposób możemy również uzyskać informacje o godzinie, minucie i sekundzie. Możemy również wykonywać operacje na dacie, na przykład dodawanie lub odejmowanie dni, godzin itp.

## Głębszy wgląd

Aby jeszcze lepiej wykorzystać możliwości typu `Date`, możemy użyć komponentów takich jak `DateFormatter` lub `DateComponents`. `DateFormatter` pozwala nam na formatowanie daty zgodnie z naszymi preferencjami, na przykład wyświetlanie jej w innym języku lub w innej strefie czasowej. Natomiast `DateComponents` pozwala nam na bardziej zaawansowane operacje na dacie, na przykład dodawanie tylko określonej ilości godzin lub minut.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o typie `Date` w Swift, możesz zapoznać się z poniższymi linkami:

- [Dokumentacja Apple o typie Date](https://developer.apple.com/documentation/foundation/date)
- [Wideo z serii "Swift Basics" omawiające typ Date](https://www.youtube.com/watch?v=Glxq-Jeymdg)
- [Przewodnik po typie Date na stronie Hacking with Swift](https://www.hackingwithswift.com/example-code/system/how-to-handle-dates-and-times-in-swift)

Teraz, gdy już wiesz jak uzyskać aktualną datę w Swift, możesz wykorzystać tę wiedzę w swoich projektach. Powodzenia!