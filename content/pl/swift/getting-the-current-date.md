---
title:                "Swift: Uzyskiwanie bieżącej daty"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często jesteśmy zmuszeni do wykorzystywania dat, szczególnie obecnej daty. Może to być potrzebne do zapisywania wydarzeń, porządkowania danych lub w celach statystycznych. Dlatego warto wiedzieć, jak uzyskać bieżącą datę w języku Swift.

## Jak to zrobić

Aby uzyskać bieżącą datę w języku Swift, możemy skorzystać z wbudowanej funkcji `Date()`. Przykładowy kod wygląda następująco:

```Swift
let currentDate = Date()
print(currentDate)
```
Output:
`2021-02-22 19:00:00 +0000`

Jeśli chcemy zmienić format wyświetlanego czasu, możemy użyć obiektu `DateFormatter` wraz z funkcją `string(from:)`. Przykładowy kod wygląda następująco:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let currentDate = Date()
print(formatter.string(from: currentDate))
```
Output:
`22/02/2021`

## Głębsze zanurzenie 

Uzyskiwanie bieżącej daty może być bardziej skomplikowane, jeśli chcemy dodatkowo uwzględnić różnicę stref czasowych czy zmianę czasu letniego/zimowego. Wówczas możemy skorzystać z klasy `Calendar`, która pozwala na dostęp do bardziej zaawansowanych funkcji związanych z datami.

Na przykład, aby uzyskać bieżącą datę w wybranej strefie czasowej, możemy użyć kodu:

```Swift
let calendar = Calendar.current
let currentDate = Date()
let selectedDate = calendar.date(byAdding: .hour, value: 3, to: currentDate) //dodanie 3 godzin
print(selectedDate)
}
```
Output:
`2021-02-22 22:00:00 +0000`

Więcej informacji o klasie `Calendar` i jej możliwościach znajdziesz w dokumentacji Apple.

## Zobacz także

- [Dokumentacja Apple - Date](https://developer.apple.com/documentation/foundation/date)
- [Dokumentacja Apple - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Dokumentacja Apple - Calendar](https://developer.apple.com/documentation/foundation/calendar)