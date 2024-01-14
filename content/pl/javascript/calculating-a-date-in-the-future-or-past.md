---
title:                "Javascript: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie to nie tylko pisanie kodu, ale także rozwiązywanie problemów. Czasem musimy policzyć datę w przyszłości lub w przeszłości, na przykład gdy pracujemy nad aplikacją kalendarza lub planujemy wydarzenia. Wtedy ważne jest, aby znać sposoby na obliczanie daty w przyszłości lub przeszłości, aby nasz kod był precyzyjny i wykorzystywał odpowiednie informacje.

## Jak to zrobić

W JavaScript istnieje wiele metod na obliczanie daty w przyszłości lub przeszłości. Możemy użyć wbudowanych metod takich jak `getDate()`, `setDate()` czy `toLocaleDateString()` lub skorzystać z bibliotek takich jak moment.js.

```Javascript
// Obliczenie daty w przyszłości
let dzis = new Date();
dzis.setDate(dzis.getDate() + 7);
console.log(dzis.toLocaleDateString());

// Obliczenie daty w przeszłości
let wczoraj = new Date();
wczoraj.setDate(wczoraj.getDate() - 1);
console.log(wczoraj.toLocaleDateString());
```

W powyższym przykładzie użyliśmy metody `setDate()` do obliczenia daty w przyszłości i przeszłości na podstawie aktualnej daty. Dzięki temu możemy wykorzystać już wbudowane funkcje języka JavaScript zamiast pisać skomplikowane algorytmy.

## Głębsze wchłonięcie

Podczas obliczania daty w przyszłości lub przeszłości musimy pamiętać o różnicach w czasie, gdyż nie wszystkie kraje czy regiony korzystają z tego samego systemu czasu. Dlatego warto poznać również takie metody jak `getTimezoneOffset()` czy `getHours()`, dzięki którym możemy precyzyjnie określić datę w zależności od strefy czasowej.

Innym ważnym aspektem jest również obsługa różnych formatów daty. W tym przypadku przydatna może być metoda `toLocaleDateString()`, która pozwala na wybór określonego formatu, np. polskiego czy amerykańskiego.

## Zobacz także

- [Oficjalna dokumentacja JavaScript o obiektach daty](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
- [Biblioteka moment.js do pracy z datami w JavaScript](https://momentjs.com/)