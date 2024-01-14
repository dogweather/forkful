---
title:                "Javascript: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego?

Porównywanie dwóch dat jest powszechnym zadaniem w pracy programisty. W myśl zasady "czas to pieniądz", operacje na datach często są niezbędne dla rozwiązywania codziennych problemów programistycznych. W tym wpisie opowiemy o tym, dlaczego warto nauczyć się porównywać daty za pomocą JavaScript.

## Jak to zrobić?

Aby porównać dwie daty w JavaScript, musimy najpierw utworzyć obiekty typu `Date` przy użyciu konstruktora oraz przypisać im odpowiednie wartości. Następnie możemy użyć operatorów logicznych i metod do porównywania dat.

```Javascript
let date1 = new Date('2020/01/01');
let date2 = new Date('2020/05/10');

// Porównanie za pomocą operatora <
if (date1 < date2) {
  console.log('Pierwsza data jest wcześniejsza od drugiej');
}

// Porównanie za pomocą metody getTime()
if (date1.getTime() < date2.getTime()) {
  console.log('Pierwsza data jest wcześniejsza od drugiej');
}

// Porównanie za pomocą metody getDate()
if (date1.getDate() < date2.getDate()) {
  console.log('Pierwsza data jest wcześniejsza od drugiej');
}

// Porównanie za pomocą metody compare()
if (date1.compare(date2) === -1) {
  console.log('Pierwsza data jest wcześniejsza od drugiej');
}
```

W powyższych przykładach używamy różnych metod do porównywania dat. Metoda `getDate()` zwraca dzień miesiąca, a metoda `getTime()` zwraca liczbę milisekund od 1 stycznia 1970 roku. Metoda `compare()` porównuje daty, zwracając -1 jeśli pierwsza data jest wcześniejsza, 0 przy równych datach oraz 1 jeśli druga data jest wcześniejsza.

## Głębsza analiza

Porównywanie dat może wydawać się prostym zadaniem, jednak warto poznać kilka ważnych koncepcji związanych z datami w JavaScript. Poniżej znajdziesz kilka przydatnych wskazówek:

- Możemy porównywać nie tylko daty, ale także godziny, minuty i sekundy. Wystarczy wykorzystać odpowiednie metody, np. `getHours()` czy `getSeconds()`.
- Data utworzona przy użyciu konstruktora `Date()` ma czas względem strefy czasowej użytkownika. Może to powodować nieporozumienia przy porównywaniu dat między różnymi użytkownikami.
- Warto pamiętać, że miesiące są numerowane od zera, czyli styczeń to miesiąc 0, luty to miesiąc 1, itd.
- JavaScript używa domyślnego formatu daty i czasu ustalonego przez użytkownika w systemie operacyjnym. Możemy zmienić ten format poprzez ustawienie odpowiednich opcji w komputerze.

## Zobacz też

- [Dokumentacja JavaScript - obiekt Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
- [Porównywanie dat w innych językach programowania](https://www.geeksforgeeks.org/date-comparison-in-javascript/)
- [Formatowanie daty w JavaScript](https://www.w3schools.com/js/js_date_formats.asp)