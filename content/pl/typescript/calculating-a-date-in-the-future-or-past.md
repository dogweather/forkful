---
title:                "TypeScript: Liczenie daty w przyszłości lub przeszłości"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być przydatne w wielu przypadkach, na przykład w aplikacjach do zarządzania zadaniami lub planowaniu spotkań. Jest to również dobry sposób, aby lepiej zrozumieć manipulację datami i czasem w programowaniu.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w TypeScript, można użyć wbudowanej metody `Date()` lub biblioteki zewnętrznej, takiej jak Moment.js. Przedstawimy przykładowe kody dla obu opcji.

### Użycie wbudowanej metody `Date()`

```
TypeScript
let today = new Date();
let futureDate = today.getDate() + 7;
console.log(futureDate); // Output: 29
```

W powyższym przykładzie, tworzymy obiekt daty dla dzisiejszego dnia za pomocą wbudowanej metody `Date()`. Następnie, korzystając z metody `getDate()`, pobieramy dzień z daty dzisiejszej i dodajemy do niego liczbę 7, aby obliczyć datę w przyszłości.

### Użycie biblioteki Moment.js

```
TypeScript
import moment from 'moment';
let today = moment();
let futureDate = today.add(7, 'days');
console.log(futureDate.format('YYYY-MM-DD')); // Output: 2019-11-29
```

W tym przykładzie, korzystamy z biblioteki Moment.js do obliczenia daty w przyszłości. Najpierw, importujemy bibliotekę i tworzymy obiekt daty dla dzisiejszego dnia. Następnie, używając metody `add()` z biblioteki, dodajemy 7 dni do daty dzisiejszej. Na koniec, używamy metody `format()` do sformatowania daty w żądanym formacie.

## Deep Dive

Obliczanie daty w przyszłości lub przeszłości może być bardziej skomplikowane, jeśli chcemy uwzględnić różne strefy czasowe, operacje arytmetyczne z datami lub przystępne formaty wyjściowe. W takim przypadku, warto zapoznać się z dokumentacją wbudowanej metody `Date()` i zwrócić szczególną uwagę na bibliotekę Moment.js, która oferuje wiele przydatnych funkcji związanych z manipulacją datami i czasem.

## Zobacz również

* [Dokumentacja wbudowanej metody Date()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [Dokumentacja biblioteki Moment.js](https://momentjs.com/docs/)