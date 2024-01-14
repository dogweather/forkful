---
title:                "TypeScript: Otrzymywanie aktualnej daty"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego warto dowiedzieć się, jaka jest data

Zastosowanie aktualnej daty w programowaniu jest nieodłącznym elementem wielu aplikacji. Jest to szczególnie ważne w przypadku tworzenia narzędzi, które śledzą lub prezentują informacje o tymczasowych zdarzeniach lub wydarzeniach z przeszłości. Poznanie aktualnej daty i czasu jest także niezbędne przy obsłudze bieżących działań na różnych urządzeniach.

## Jak to zrobić w TypeScripcie

Przed przystąpieniem do kodowania, należy zaimportować moduł `Date` w pliku TypeScript:

```TypeScript
import { Date } from 'Date';
```

Następnie można stworzyć nową instancję `Date` i przypisać ją do zmiennej, aby otrzymać aktualną datę:

```TypeScript
const currentDate = new Date();
```

Aby wyświetlić aktualną datę w konsoli, można wykorzystać metodę `toLocaleDateString()`:

```TypeScript
console.log(currentDate.toLocaleDateString());
```

Można także wyświetlić dokładną godzinę, wykorzystując metodę `toLocaleTimeString()`:

```TypeScript
console.log(currentDate.toLocaleTimeString());
```

Kod w całości może wyglądać tak:

``` TypeScript
import { Date } from 'Date';

const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
console.log(currentDate.toLocaleTimeString());
```

Po uruchomieniu powyższego kodu, w konsoli powinna pojawić się aktualna data i godzina w formacie ustawionym w systemie operacyjnym.

## Głębsze zanurzenie

W bibliotece `Date` dostępnych jest wiele metod, które pozwalają na dokładniejsze manipulowanie aktualną datą. Aby poznać więcej możliwości, warto zagłębić się w dokumentację [biblioteki Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date).

## Zobacz także

- [Dokumentacja biblioteki Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
- [Poradnik programowania w TypeScript](https://auth0.com/blog/typescript-tutorial-for-javascript-developers/)
- [Tworzenie aplikacji z u