---
title:    "TypeScript: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości jest bardzo przydatne w programowaniu. Może pomóc w wyświetlaniu terminów lub dat wydarzeń, tworzeniu harmonogramów lub wyznaczaniu czasowych ograniczeń dla aplikacji. Jest to również ważne, ponieważ dostarcza informacji o datach, które mogą być trudne lub nieintuicyjne do obliczenia ręcznie.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w TypeScript, musisz użyć wbudowanej klasy `Date`. Możesz użyć różnych metod tej klasy, aby dodawać lub odejmować dni, miesiące, lata lub inne jednostki czasu od bieżącej daty. Następnie możesz użyć metody `toLocaleDateString()` lub `toLocaleString()` w celu sformatowania wyjścia według preferencji językowych użytkownika. Przykładowy kod wyglądałby następująco:

```
TypeScript
let currentDate = new Date();
let futureDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), currentDate.getDate() + 14);
console.log(futureDate.toLocaleDateString()); // wyświetli datę dwa tygodnie od bieżącej daty, sformatowaną według preferencji językowych użytkownika
```

Output:

```
"21.06.2021"
```

## Deep Dive

W tym przykładzie użyliśmy metody `getDate()` klasy `Date`, aby dodać 14 dni do bieżącej daty. Jednak istnieje wiele innych metod, które mogą być wykorzystane do obliczania dat w przyszłości lub przeszłości. Na przykład, można użyć metody `setFullYear()`, aby ustawiać rok, `setMonth()` dla miesięcy lub `setDate()` dla dni. Możliwości są praktycznie nieograniczone, a w zależności od potrzeb programisty, różne kombinacje metod mogą być wykorzystane do osiągnięcia pożądanego wyniku.

## Zobacz również

Jeśli jesteś zainteresowany/a dalszym zgłębianiem tematu obliczania dat w przyszłości lub przeszłości w TypeScript, polecamy Ci zapoznać się z następującymi linkami:

- [Dokumentacja TypeScript dla klasy Date](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [Artykuł na temat obliczania dat w TypeScript](https://www.digitalocean.com/community/tutorials/how-to-code-a-date-picker)
- [Tutorial na temat manipulacji datami w TypeScript](https://www.digitalocean.com/community/tutorials/manipulating-dates-and-times-in-javascript-with-moment-js)