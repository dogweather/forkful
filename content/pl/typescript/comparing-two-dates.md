---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Porównywanie dwóch dat to proces ustalania, która data jest wcześniejsza, a która późniejsza. Programiści robią to, gdy potrzebują operować na danych związanych z czasem i datą, na przykład do sortowania wydarzeń wg daty, terminach ważności sesji użytkownika, itp.

## Jak zrobić:

Aby porównać dwie daty w TypeScript, tworzymy dwa obiekty `Date` i wykorzystujemy metodę `getTime()`, która zwraca czas w milisekundach, liczone od 1 stycznia 1970 roku.

```TypeScript
let data1: Date = new Date('2021-03-01');
let data2: Date = new Date('2021-03-02');

if(data1.getTime() < data2.getTime()) {
    console.log("Data1 jest wcześniejsza niż Data2");
} else if(data1.getTime() > data2.getTime()) {
    console.log("Data1 jest późniejsza niż Data2");
} else {
    console.log("Data1 i Data2 są równe");
}
```
Sample output:

```TypeScript
"Data1 jest wcześniejsza niż Data2"
```
## Pogłębione omówienie:

Porównywanie dat jest dość prostym procesem, jednak ważne jest zrozumienie, jak jest zaimplementowane w TypeScript. Mamy dwie daty, które porównujemy używając metody `getTime()`. Metoda ta zwraca wartość czasu w milisekundach, dzięki czemu możemy łatwo porównać daty.

Dodatkowo, warto wiedzieć, że istnieją alternatywne metody porównywania dat. Na przykład, zamiast konwertować do milisekund, można porównać rok, miesiąc i dzień separatystycznie. Choć może to być bardziej skomplikowane, czasami może być preferowane w zależności od scenerio.

## Zobacz także:

1. Dokumentacja MDN na temat obiektu Date: https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date
2. Wizualizator JavaScript Date: https://www.w3schools.com/js/js_dates.asp
3. Artykuł na temat porównywania dat w TypeScript: https://www.freecodecamp.org/news/how-to-compare-dates-in-typescript/