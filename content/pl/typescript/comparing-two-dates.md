---
title:    "TypeScript: Porównywanie dwóch dat"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Czas jest nieodzownym aspektem programowania i często musimy porównywać różne daty do wykonania określonych operacji. Porównanie dat może pomóc nam w ustaleniu, która data jest wcześniejsza lub późniejsza oraz w wykonaniu działań, takich jak obliczanie różnicy między dwoma datami. W tym wpisie dowiecie się, jak w łatwy sposób porównywać daty w języku TypeScript.

## Jak to zrobić

Najprostszym sposobem na porównywanie dat w TypeScript jest wykorzystanie wbudowanych funkcji takich jak `Date()` i `getTime()`. Przykładowy kod wyglądałby mniej więcej tak:

```TypeScript
let data1 = new Date("2021-01-01");
let data2 = new Date("2020-12-25");

if(data1.getTime() > data2.getTime()){
  console.log("Data 1 jest późniejsza niż data 2");
} else if (data1.getTime() < data2.getTime()){
  console.log("Data 1 jest wcześniejsza niż data 2");
} else {
  console.log("Obie daty są takie same");
}
```

W powyższym przykładzie tworzymy dwie zmienne, `data1` i `data2`, które przechowują daty w formacie "rok-miesiąc-dzień". Następnie wykorzystujemy funkcję `getTime()`, która zwraca wartość liczbową reprezentującą datę, umożliwiającą jej porównanie. W zależności od wyniku porównania, wypisujemy odpowiedni komunikat w konsoli.

## Deep Dive

Dodatkowo, możemy wykorzystać biblioteki takie jak Moment.js lub date-fns, które zapewniają nam bardziej rozbudowane i precyzyjne metody porównywania dat. Przykładowy kod wykorzystujący bibliotekę Moment.js wyglądałby tak:

```TypeScript
import moment from 'moment';

let data1 = moment("2021-01-01", "YYYY-MM-DD");
let data2 = moment("2020-12-25", "YYYY-MM-DD");

if(data1.isAfter(data2)){
  console.log("Data 1 jest późniejsza niż data 2");
} else if (data1.isBefore(data2)){
  console.log("Data 1 jest wcześniejsza niż data 2");
} else {
  console.log("Obie daty są takie same");
}
```

W powyższym kodzie używamy funkcji `isAfter()` i `isBefore()` biblioteki Moment.js do porównania dat. Zachęcamy do dokładnego zapoznania się z dokumentacją tych bibliotek, aby poznać więcej możliwości i funkcjonalności.

## Zobacz także

- [Dokumentacja biblioteki Moment.js](https://momentjs.com/docs/)
- [Dokumentacja biblioteki date-fns](https://date-fns.org/docs/)