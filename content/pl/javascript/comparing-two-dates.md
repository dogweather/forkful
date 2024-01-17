---
title:                "Porównywanie dwóch dat"
html_title:           "Javascript: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Porównywanie dwóch dat jest procesem, który polega na sprawdzeniu, która z dwóch podanych dat jest wcześniejsza lub późniejsza. Jest to bardzo ważna funkcja w programowaniu, ponieważ pozwala nam na efektywne zarządzanie różnymi zdarzeniami w naszym programie, takimi jak wygaśnięcie abonamentów, ważność kodów rabatowych lub daty urodzin użytkowników.

## Jak to zrobić:
```javascript
const date1 = new Date(2021, 1, 1);
const date2 = new Date(2021, 3, 1);

if (date1 < date2) {
  console.log("Date 1 comes before Date 2");
} else if (date1 > date2) {
  console.log("Date 2 comes before Date 1");
} else {
  console.log("Date 1 and Date 2 are equal");
}
```
### Wynik:
`Date 1 comes before Date 2`

## Głębszy wgląd:
1. Kontekst historyczny:
Porównywanie dwóch dat jest procesem, który był wykorzystywany już w czasach komputeryzacji. Wcześniej, kiedy używano tylko liczb, porównywanie dat było trudniejsze. Dzięki łatwiejszej pracy z datami w programowaniu, ten proces stał się znacznie prostszy.

2. Alternatywy:
W Javascript istnieją różne funkcje, które można wykorzystać do porównywania dat, takie jak `getTime()`, `valueOf()`, `getDate()`. Każda z nich ma swoje wady i korzystanie z nich może być zależne od specyfiki danego zadania.

3. Szczegóły implementacji:
Podczas porównywania dat, Javascript wykorzystuje wartość liczbową time stamp, która reprezentuje ilość milisekund, które minęły od 1 stycznia 1970 roku do danej daty. Dzięki temu, porównywanie dwóch dat jest możliwe przez porównanie wartości time stamp.

## Zobacz również:
- [JavaScript Date Methods](https://www.w3schools.com/js/js_date_methods.asp)
- [Comparing Dates in JavaScript](https://dmitripavlutin.com/how-to-compare-dates-in-javascript/)