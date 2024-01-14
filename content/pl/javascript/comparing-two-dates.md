---
title:                "Javascript: Porównywanie dwóch dat"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie to sztuka rozwiązywania problemów, a jednym z powszechnych problemów jest porównywanie dwóch dat. Porównywanie dat jest ważnym elementem w wielu aplikacjach, takich jak systemy przewodzenia dokumentów czy też tworzenie kalendarzy. W tym artykule dowiesz się, w jaki sposób porównywać dwie daty w języku JavaScript.

## Jak To Zrobić

Porównywanie dwóch dat w JavaScript może wydawać się skomplikowane, jednak dzięki wbudowanym funkcjom możemy to zrobić w prosty sposób. Przykładem takiej funkcji jest `Date.parse()`, która konwertuje datę z postaci tekstowej na liczbę reprezentującą liczbę milisekund od 1 stycznia 1970 roku. W tym przypadku, im większa liczba milisekund, tym późniejsza data.

```Javascript
const data1 = new Date('2021-01-01');
const data2 = new Date('2021-01-02');

console.log(data1); //Wed Dec 31 2020 19:00:00 GMT-0500
console.log(data2); //Fri Jan 01 2021 19:00:00 GMT-0500

const milisekundyData1 = Date.parse(data1);
const milisekundyData2 = Date.parse(data2);

console.log(milisekundyData1); //1609455600000
console.log(milisekundyData2); //1609542000000

//Porównanie dat
if (milisekundyData1 < milisekundyData2) {
  console.log("Data 1 jest wcześniejsza od daty 2");
} else if (milisekundyData1 > milisekundyData2) {
  console.log("Data 2 jest wcześniejsza od daty 1");
} else {
  console.log("Daty są identyczne");
}
```

W powyższym przykładzie, porównujemy daty `data1` i `data2` poprzez konwersję ich na liczbę milisekund i porównanie tych liczb. Możemy również porównać daty bezpośrednio, używając operatorów porównania `<`, `>` lub `===`.

## Deep Dive

W języku JavaScript, każda konkretna data jest ustawiona na czas lokalny użytkownika i jest wyświetlana w jego strefie czasowej. Jednak przy porównywaniu dat, ważne jest, aby ustalić strefę czasową, ponieważ inaczej porównania mogą być niepoprawne. W takich przypadkach, warto korzystać z funkcji `getUTC*()` i `setUTC*()`, które są ustawione na czas uniwersalny (UTC).

Innym aspektem do wzięcia pod uwagę jest, że funkcja `Date.parse()` może mieć różne zachowanie w różnych przeglądarkach. Dlatego zaleca się korzystanie z bibliotek, takich jak Moment.js, które ułatwiają pracę z datami w języku JavaScript.

## Zobacz również

- [Date.parse()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Obiekty_globalne/Date/parse)
- [Moment.js](https://momentjs.com/)
- [Światowy zegar](https://www.timeanddate.com/worldclock/)