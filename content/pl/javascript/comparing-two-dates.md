---
title:    "Javascript: Porównywanie dwóch dat"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Czas jest jednym z najważniejszych elementów programowania, a często potrzebujemy porównania dwóch dat w naszym kodzie. Może to być przydatne w różnych sytuacjach, takich jak ustalanie, którą datę należy wyświetlić na stronie lub weryfikacja, czy dany dzień jest wcześniejszy czy późniejszy od innego. W tym artykule dowiesz się, jak łatwo porównywać daty w języku Javascript.

## Jak porównywać daty w Javascript

Aby porównać dwie daty w języku Javascript, musimy najpierw utworzyć obiekty daty przy użyciu konstruktora `new Date()`. Następnie możemy wykorzystać różne metody, aby przeprowadzić porównanie. Oto przykładowy kod:

```Javascript
let date1 = new Date("June 1, 2021");
let date2 = new Date("May 1, 2021");

// Porównaj daty za pomocą operatora porównania "=="
if (date1 == date2) {
  console.log("Daty są równe");
} else {
  console.log("Daty są różne");
}

// Porównaj daty za pomocą metody "getTime()" - zwraca czas w milisekundach od 1 stycznia 1970 r.
if (date1.getTime() == date2.getTime()) {
  console.log("Daty są równe");
} else {
  console.log("Daty są różne");
}

// Porównaj daty za pomocą operatora porównania "<"
if (date1 < date2) {
  console.log("Date 1 jest wcześniejsza niż data 2");
} else {
  console.log("Data 2 jest wcześniejsza niż data 1");
}
```

Wynik:

```
Daty są różne
Daty są różne
Date 2 jest wcześniejsza niż data 1
```

## Głębsze spojrzenie

Aby dokładniej zrozumieć, jak działa porównywanie dat w Javascript, warto wiedzieć, że obiekty daty są przechowywane w formacie czasu Unix, czyli liczby reprezentującej liczbę sekund od 1 stycznia 1970 r. W związku z tym, porównujemy nie same daty, ale ich wartości numeryczne. Dlatego też metoda `getTime()` jest tak przydatna - sprawdzając, czy wartości są identyczne, możemy stwierdzić, czy dwie daty są takie same.

Ponadto, niektóre metody obiektów daty mogą nam pomóc w bardziej szczegółowym porównywaniu, na przykład `getDay()` zwraca dzień tygodnia dla danej daty, a `getMonth()` zwraca miesiąc. Możemy wykorzystać te dane do precyzyjnego ustalenia, która data jest wcześniejsza lub późniejsza.

## Zobacz również

- [Dokumentacja JavaScipt na temat obiektu Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Porównywanie dat w programowaniu](https://medium.com/javascript-in-plain-english/comparing-dates-in-programming-a51e212249cd)
- [Przydatne wskazówki dotyczące obiektu Date w JavaScript](https://www.digitalocean.com/community/tutorials/decimalbase-how-to-work-with-dates-and-times-in-javascript)