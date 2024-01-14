---
title:    "Javascript: Porównywanie dwóch dat"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest jednym z kluczowych aspektów w programowaniu. Jest to niezbędne w wielu zastosowaniach, takich jak obliczanie długości trwania zdarzeń, kalkulacje czasu czy sortowanie danych. Używanie wbudowanych funkcji w Javascript do porównywania dat jest nie tylko szybkie i wygodne, ale także zapewnia dokładne wyniki. W tym artykule dowiesz się, jak porównywać dwie daty w języku Javascript.

## Jak to zrobić

Poniżej znajdują się przykłady kodów i wyników porównywania dwóch dat w języku Javascript.

### Przykład 1: Porównanie daty z użyciem operatorów porównania

```Javascript
let date1 = new Date("2021-07-21");
let date2 = new Date("2021-07-25");

if (date1 < date2) {
  console.log("Pierwsza data jest wcześniej od drugiej daty");
} else if (date1 > date2) {
  console.log("Pierwsza data jest później od drugiej daty");
} else {
  console.log("Obie daty są takie same");
}

// Wynik: Pierwsza data jest wcześniej od drugiej daty
```

W tym przykładzie wykorzystujemy operatory porównania (mniejsze niż i większe niż) do porównania dwóch dat. Wynik jest wyświetlany w konsoli w zależności od tego, która data jest wcześniej lub później.

### Przykład 2: Porównanie daty z użyciem funkcji getTime()

```Javascript
let date1 = new Date("2021-09-01");
let date2 = new Date("2021-09-01");

if (date1.getTime() === date2.getTime()) {
  console.log("Obie daty są takie same");
} else {
  console.log("Daty są różne");
}

// Wynik: Obie daty są takie same
```

W tym przykładzie wykorzystujemy metodę `getTime()` dla obu dat, która zwraca ilość milisekund od 1 stycznia 1970 roku. Następnie porównujemy wartości zwrócone przez tę metodę, aby stwierdzić czy daty są takie same czy różne.

## Deep Dive

Porównywanie dat może nie być oczywiste dla początkujących programistów. Istnieje wiele innych metod i funkcji w języku Javascript, które mogą być użyte do porównania dat, takich jak `getFullYear()`, `getDate()` czy `getMonth()`. Ważne jest, aby zawsze przetestować swoją implementację porównywania dat, aby upewnić się, że działa ona zgodnie z oczekiwaniami.

## Zobacz także

- Porównywanie dat w języku Javascript: https://www.w3schools.com/js/js_date_methods.asp
- Przydatna biblioteka do operacji na datach w Javascript: https://momentjs.com/
- Praktyczne przykłady porównywania dat w różnych sytuacjach: https://codeburst.io/javascript-dates-557983222d98