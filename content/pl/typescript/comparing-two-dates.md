---
title:                "Porównywanie dwóch dat"
html_title:           "TypeScript: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest częstą operacją w programowaniu, której celem jest sprawdzenie, która data jest wcześniejsza lub późniejsza. W języku TypeScript istnieje kilka metod, które umożliwiają dokonanie tego porównania. W tym artykule poznamy te metody oraz dowiemy się, jak można je wykorzystać w praktyce.

## Jak

Porównywanie dwóch dat w TypeScript jest bardzo proste. Przedstawimy tutaj dwa sposoby, które pozwolą nam porównać daty i uzyskać pożądany wynik.

```TypeScript
const data1: Date = new Date('2020-01-01');
const data2: Date = new Date('2021-01-01');

console.log(data1 > data2); // false
console.log(data1 < data2); // true
console.log(data1.getTime() === data2.getTime()); // false
```

W powyższym przykładzie utworzyliśmy dwie zmienne zawierające daty i za pomocą operatorów porównania `>` i `<` sprawdziliśmy, która jest późniejsza. Możemy również użyć metody `getTime()` do uzyskania wartości w formie liczby, która następnie jest porównywana za pomocą operatora `===`.

Innym sposobem jest wykorzystanie metody `compare()` z biblioteki `moment.js`.

```TypeScript
import moment from 'moment';

const data1: Date = new Date('2020-01-01');
const data2: Date = new Date('2021-01-01');

console.log(moment(data1).isBefore(data2)); // true
console.log(moment(data2).isAfter(data1)); // true
console.log(moment(data1).isSame(data2)); // false
```

Wykorzystując bibliotekę `moment.js` możemy porównywać daty za pomocą wygodnych metod, które zwracają wartość logiczną.

## Deep Dive

Podczas porównywania dat nie tylko sprawdzamy, która jest późniejsza, ale również uwzględniamy czas i strefę czasową. W bibliotece `moment.js` możemy wykorzystać metodę `isSame()` z dodatkowym argumentem, który określa jednostkę czasu. Dzięki temu możemy porównać daty z dokładnością do sekund, minut, godzin itp.

```TypeScript
import moment from 'moment';

const data1: Date = new Date('2021-05-01');
const data2: Date = new Date('2021-05-01 12:00:00');

console.log(moment(data1).isSame(data2, 'second')); // false
console.log(moment(data2).isSame(data2, 'day')); // true
```

Dodatkowo, podczas porównywania dat w TypeScript, warto pamiętać, że obiekt `Date` przechowuje również informację o strefie czasowej. Może to wpłynąć na wynik porównania, dlatego ważne jest, aby upewnić się, że obie daty są w tej samej strefie czasowej.

## Zobacz także

1. Dokumentacja biblioteki `moment.js`: https://momentjs.com/docs/
2. Porównywanie dat w języku JavaScript: https://www.w3schools.com/js/js_dates.asp
3. Porównywanie dat w języku TypeScript: https://www.typescriptlang.org/docs/handbook/declaration-merging.html