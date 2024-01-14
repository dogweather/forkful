---
title:    "Swift: Konwertowanie daty na ciąg znaków"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na łańcuch znaków jest częstym zadaniem w programowaniu, szczególnie w języku Swift. Jest to przydatna umiejętność, która pozwala na wyświetlanie dat w czytelny sposób dla użytkownika. W tym blogu dowiecie się, jak łatwo i szybko przekształcić datę na łańcuch znaków.

## Jak to zrobić

Konwersja daty na łańcuch znaków jest możliwa dzięki użyciu metody `String(describing:)`. Wystarczy podać obiekt typu `Date` jako jej argument, a zostanie ona automatycznie przekonwertowana na łańcuch zgodnie z ustawionym formatem. Przykładowy kod wygląda następująco:

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
let dateString = String(describing: date)
print(dateString)
```

Powinniśmy uzyskać wyjście w postaci np. "24-09-2021".

Dzięki temu prostemu rozwiązaniu, możemy dostosować format wyświetlania daty do swoich preferencji. Warto również wspomnieć, że metoda `String(describing:)` działa również z innymi typami danych, np. `Int` czy `Float`.

## Głębszy wgląd

Konwertowanie daty na łańcuch może wydawać się prostym zadaniem, ale warto poznać bardziej zaawansowane metody, które dają większą kontrolę nad wyświetlaniem dat. W celu uzyskania pełnego formatu daty, używamy metody `string(from:)` z obiektem `DateFormatter`. Przykładowy kod wykorzystujący tę metodę wygląda następująco:

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .long
dateFormatter.timeStyle = .medium
let dateString = dateFormatter.string(from: date)
print(dateString)
```

Wyjście powinno wyglądać np. "24 września 2021, 14:30:00".

Oprócz ustawienia stylów daty i czasu, istnieje wiele innych opcji, takich jak ustawianie języka, lokalizacji czy dostęp do poszczególnych elementów daty, takich jak dzień, miesiąc czy rok.

## Zobacz też

- [Dokumentacja Swift - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Przekształcanie daty w łańcuch znaków w języku Swift](https://learnappmaking.com/date-string-swift-how-to/)

Konwertowanie daty na łańcuch znaków jest podstawową umiejętnością, którą powinien znać każdy programista w języku Swift. Dzięki temu artykułowi, mam nadzieję, że będziesz w stanie wykorzystać tę funkcjonalność w swoich projektach. Zawsze pamiętaj, by dostosować format wyświetlania daty do potrzeb użytkownika i nie zapomnij sprawdzić dostępnych opcji, aby uzyskać jeszcze większą kontrolę nad wyglądem daty.