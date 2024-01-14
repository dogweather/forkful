---
title:                "TypeScript: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z najważniejszych umiejętności programowania jest manipulacja datami. Często musimy obliczyć datę w przyszłości lub przeszłości, na przykład w celu wyświetlenia terminu ważności karty kredytowej lub obliczenia wieku użytkownika. W tym artykule dowiesz się, jak w prosty sposób obliczać daty w przyszłości lub przeszłości za pomocą języka TypeScript. 

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w TypeScript, musimy użyć obiektu Date, który jest wbudowany w język JavaScript. Ten obiekt zawiera różne metody, dzięki którym możemy manipulować datami.

Przykładowo, jeśli chcemy obliczyć datę 7 dni od dzisiaj lub dzisiaj 7 dni temu, możemy użyć metody `getDate()` w połączeniu z metodą `setDate()`.

```TypeScript
// Obliczenie daty 7 dni w przód
let today = new Date();
today.setDate(today.getDate() + 7);
console.log(today); // Output: Tue Dec 08 2021

// Obliczenie daty 7 dni wstecz
let today = new Date();
today.setDate(today.getDate() - 7);
console.log(today); // Output: Tue Nov 24 2021
```

Podobnie, możemy również dostosować daty w przyszłości lub przeszłości poprzez ustawienie odpowiednich wartości dla roku, miesiąca i dnia za pomocą metod `setFullYear()`, `setMonth()` i `setDate()`.

```TypeScript
// Ustawienie daty za pomocą metod setDate(), setMonth() i setFullYear()
let date = new Date();
date.setDate(15);
date.setMonth(11);
date.setFullYear(2022);
console.log(date); // Output: Tue Dec 15 2022
```

## Deep Dive

Obliczanie daty w przyszłości lub przeszłości może być nieco bardziej skomplikowane w przypadku dat, które znajdują się na przecięciu dwóch różnych miesięcy lub lat. W takich przypadkach zalecamy skorzystanie z biblioteki moment.js, która udostępnia wiele wygodnych funkcji do pracy z datami.

Na przykład, aby obliczyć datę 45 dni od daty 28 listopada, musielibyśmy najpierw utworzyć obiekt date z tą datą, a następnie użyć metody `add()` z biblioteki moment.js.

```TypeScript
// Użycie biblioteki moment.js do obliczenia daty 45 dni w przód
let date = moment("2021-11-28");
date.add(45, 'days');
console.log(date); // Output: Sat Jan 11 2022
```

W ten sposób możemy wygodnie manipulować datami w różnych formatach i przeprowadzać bardziej złożone obliczenia.

## Zobacz również

- Dokumentacja TypeScript dotycząca obiektu Date: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-9.html#-widen-the-types-of-the-year-parameter-in-date-setfullyear
- Biblioteka moment.js: https://momentjs.com/