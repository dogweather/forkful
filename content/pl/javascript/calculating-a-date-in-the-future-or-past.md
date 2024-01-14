---
title:                "Javascript: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może okazać się bardzo przydatne podczas tworzenia różnego rodzaju aplikacji. Może to być przydatne w takich przypadkach, jak wyświetlanie terminów ważności, planowania wydarzeń lub kontrolowania danych historycznych.

## Jak to zrobić

```javascript
// Obliczanie daty w przyszłości
let dzisiaj = new Date();
let dataWKolejnychDniach = new Date();
dataWKolejnychDniach.setDate(dzisiaj.getDate() + 7);
console.log(dataWKolejnychDniach);

// Output: 2021-08-24T16:53:48.335Z
```

```javascript
// Obliczanie daty w przeszłości
let dzisiaj = new Date();
let dataWPoprzednichDniach = new Date();
dataWPoprzednichDniach.setDate(dzisiaj.getDate() - 7);
console.log(dataWPoprzednichDniach);

// Output: 2021-08-10T16:53:48.335Z
```

Konstruktor `Date` w Javascripcie pozwala na tworzenie daty i czasu w oparciu o różne parametry, takie jak rok, miesiąc, dzień, godzina, minuta itp. W przypadku obliczania daty w przyszłości lub przeszłości, możemy skorzystać z metody `setDate()` i określić liczbę dni, o które chcemy zmienić naszą aktualną datę.

## Deep Dive

Ważną rzeczą, o której należy pamiętać przy obliczaniu daty w przyszłości lub przeszłości, jest uwzględnienie różnic czasowych. W przypadku, gdy nasza aplikacja jest wykorzystywana w różnych strefach czasowych, może to wpłynąć na dokładność obliczonej daty. W takiej sytuacji, powinniśmy skorzystać z metod `getUTC*()` i `setUTC*()`, które uwzględniają czas uniwersalny UTC.

Kolejną możliwością jest użycie zewnętrznych bibliotek, takich jak moment.js, które oferują wygodne metody do manipulowania datami i czasem. Dzięki temu można uniknąć błędów i uwzględnić różnice czasowe.

## Zobacz również

- [Dokumentacja Javascript Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)