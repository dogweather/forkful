---
title:    "Swift: Otrzymywanie bieżącej daty"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w Swift jest fascynującym doświadczeniem, ponieważ język ten jest łatwy w użyciu i wydajny. Jedną z przydatnych funkcji w Swift jest możliwość uzyskania aktualnej daty w kodzie. W tym artykule dowiesz się, jak łatwo pobrać bieżącą datę w języku Swift.

## Jak to zrobić

Podczas pisania aplikacji, często musimy uzyskać aktualną datę i wykorzystać ją do różnych obliczeń lub wyświetlenia jej użytkownikowi. W języku Swift można to zrobić przy użyciu wbudowanej funkcji ```Date()``` wraz z klasy ```DateFormatter``` do formatowania daty.

Przykładowo, aby wyświetlić bieżącą datę w formacie "dd-MM-yyyy", wystarczy wykorzystać następujący kod:

```
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
print(dateFormatter.string(from: currentDate))
```

Ten kod wyświetli aktualną datę w wybranym formacie: "02-07-2021".

Możesz również użyć innych formatów, takich jak "yyyy-MM-dd" lub "dd/MM/yyyy". Pełna lista dostępnych formatów jest dostępna w dokumentacji języka Swift.

## Głębszy wgląd

Podczas pobierania aktualnej daty w Swift warto również pamiętać o ustawieniu strefy czasowej na urządzeniu. Możesz to zrobić, wywołując metodę ```setTimeZone()``` na obiekcie ```DateFormatter```. Dzięki temu możesz zapewnić, że twoja aplikacja wyświetli poprawną datę, niezależnie od strefy czasowej urządzenia, na którym jest uruchomiona.

## Zobacz również

Możesz dowiedzieć się więcej o funkcji ```Date()``` i klasie ```DateFormatter``` w dokumentacji języka Swift.

- [Date - Swift Standard Library](https://developer.apple.com/documentation/swift/date)
- [DateFormatter - Swift Standard Library](https://developer.apple.com/documentation/swift/dateformatter)