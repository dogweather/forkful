---
title:    "Elm: Konwertowanie daty na ciąg znaków"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym procesem w wielu aplikacjach internetowych. Oznacza to przekształcenie niezrozumiałego dla człowieka formatu daty (np. 2020-05-15) na czytelną formę (np. 15 maja 2020). Jest to szczególnie przydatne, jeśli potrzebujemy wyświetlić datę dla użytkownika w prosty sposób.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w Elm, należy skorzystać z funkcji `Date.toString`. Przyjmujemy jako argument datę, którą chcemy przekonwertować oraz format, w jakim chcemy ją wyświetlić. Ważne jest, aby pamiętać, że Elm ma bardzo surowe zasady formatowania dat i godzin. Na przykład, aby wyświetlić datę w formacie DD/MM/YYYY, musimy określić to jako "yyyy/MM/dd".

```Elm
Date.toString date "yyyy/MM/dd"
```

Przykładowy wynik dla daty 15 maja 2020 będzie wyglądał tak: "2020/05/15".

## Dogłębna analiza

Istnieje wiele różnych formatów, w jakich możemy wyświetlać datę z pomocą funkcji `Date.toString`. W tym celu możemy skorzystać z różnych kombinacji literek, które odpowiadają za konkretne elementy daty i czasu. Oto kilka przykładowych kombinacji i ich odpowiednie znaczenie:

- `yyyy` - rok w formacie czterocyfrowym
- `MM` - miesiąc (z wiodącym zerem)
- `dd` - dzień (z wiodącym zerem)
- `EEEE` - dzień tygodnia w formacie pełnej nazwy (np. "piątek")
- `hh` - godziny (w formacie 12-godzinnym)
- `HH` - godziny (w formacie 24-godzinnym)
- `mm` - minuty
- `ss` - sekundy

Możemy również dodawać inne znaki, takie jak kropki czy myślniki, aby uatrakcyjnić wyświetlaną datę. Ważne jest jednak, aby pamiętać o wymogu używania konkretnych liter i symboli, ponieważ w przeciwnym razie funkcja `Date.toString` może zwrócić błąd.

## Zobacz także

- [Dokumentacja Elm dla funkcji `Date.toString`](https://package.elm-lang.org/packages/elm-lang/core/1.0.3/Date#toString)
- [Przekształcanie daty na tekst w JavaScript (w języku angielskim)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)