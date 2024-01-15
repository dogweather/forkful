---
title:                "Obliczanie daty w przyszłości lub przeszłości."
html_title:           "TypeScript: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Kalkulacja daty w przyszłości lub przeszłości może być niezbędna w przypadku wielu aplikacji, takich jak planowanie spotkań, rezerwacja biletów lub wyświetlanie okresów wypożyczeń. W przypadku języka TypeScript istnieje wiele prostych i wydajnych sposobów na to, aby łatwo przeliczać daty.

## Jak to zrobić

Obliczanie daty w przyszłości lub przeszłości w języku TypeScript jest bardzo proste i wymaga użycia jednej z wbudowanych metod obiektu `Date`. W poniższym przykładzie obliczamy datę, która jest 7 dni w przyszłości i wypisujemy ją w postaci dd-mm-yyyy.

```TypeScript
let currentDate = new Date();
currentDate.setDate(currentDate.getDate() + 7);
let futureDate = currentDate.getDate() + "-" + (currentDate.getMonth()+1) + "-" + currentDate.getFullYear();
console.log(futureDate);
```

Output: 15-10-2021

W powyższym kodzie używamy `setDate()` do ustawienia daty na 7 dni w przód, a następnie wykorzystujemy wbudowane metody `getDate()`, `getMonth()` i `getFullYear()` do pobrania odpowiednich wartości i wyświetlenia daty w oczekiwanym formacie.

## Głębszy zanurzenie

Obiekt `Date` w TypeScript oferuje wiele innych metod do manipulacji datami, takich jak `setMonth()`, `setFullYear()` czy `setHours()`, które pozwalają dostosować datę do własnych potrzeb. Dodatkowo, warto zapoznać się z bibliotekami, takimi jak `moment.js`, które są przeznaczone specjalnie do manipulacji datami i mogą ułatwić pracę z datami w projekcie.

## Zobacz również

- https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date
- https://momentjs.com/
- https://www.npmjs.com/package/date-fns