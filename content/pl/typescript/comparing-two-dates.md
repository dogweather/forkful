---
title:                "TypeScript: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat to nieodłączna część programowania w języku TypeScript. Jest to niezbędne do wielu zadań, takich jak sortowanie danych, wyświetlanie informacji w określonej kolejności lub ustalanie, czy dany dzień jest wcześniejszy czy późniejszy w porównaniu do innego. W tym wpisie dowiesz się, dlaczego jest to ważne oraz jak to zrobić w praktyce.

## Jak to zrobić

Aby porównać dwie daty w języku TypeScript, możemy skorzystać z metody `Date.prototype.getTime()`, która zwraca wartość liczbową reprezentującą liczbę milisekund od północy 1 stycznia 1970 roku. Następnie możemy porównać te wartości za pomocą operatorów porównania (np. `>`, `<` lub `===`) i ustalić, która data jest wcześniejsza lub późniejsza.

Przykładowy kod wyglądałby następująco:

```TypeScript
const firstDate = new Date(2021, 9, 10); // 10 października 2021 r.
const secondDate = new Date(2021, 9, 15); // 15 października 2021 r.

if(firstDate.getTime() > secondDate.getTime()) {
    console.log("Pierwsza data jest późniejsza.");
} else if(firstDate.getTime() < secondDate.getTime()) {
    console.log("Druga data jest późniejsza.");
} else {
    console.log("Obie daty są takie same.");
}

// Output:
// "Druga data jest późniejsza."
```

## Deep Dive

Podczas porównywania dat, istotne jest zrozumienie, że obiekty typu `Date` są mutowalne. Oznacza to, że zmiana wartości obiektu `Date` może spowodować zmianę wyniku porównania. Dlatego zaleca się porównywanie wartości zwracanych przez metodę `getTime()`, a nie samych obiektów `Date`.

Co więcej, porównywanie dwóch dat może być bardziej skomplikowane, gdy uwzględniamy różne strefy czasowe lub letnie czasowe. W takich przypadkach warto rozważyć użycie biblioteki do obsługi dat, takiej jak *Moment.js*, która oferuje szeroki zakres metod do porównywania i manipulowania danymi.

## Zobacz również

- [Dokumentacja Date.prototype.getTime()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date/getTime)
- [Porównywanie dat w języku JavaScript](https://www.digitalocean.com/community/tutorials/how-to-compare-dates-in-javascript)
- [Biblioteka Moment.js](https://momentjs.com/)