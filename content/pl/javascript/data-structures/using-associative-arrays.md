---
title:                "Korzystanie z tablic asocjacyjnych"
date:                  2024-01-30T19:12:09.587143-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tablice asocjacyjne, czy też dokładniej w kontekście JavaScript, obiekty, pozwalają na mapowanie kluczy do wartości. Jest to niezwykle przydatne, gdy potrzebujemy kolekcji elementów, do których chcemy mieć dostęp przez określone nazwy (klucze) zamiast numerycznych indeksów, co czyni kod bardziej czytelnym i elastycznym.

## Jak to zrobić:

Tworzenie i używanie tablic asocjacyjnych (obiektów) w JavaScript jest proste. Definiuje się obiekt za pomocą nawiasów klamrowych `{}`, a wewnątrz nich można zdefiniować zestaw par klucz-wartość. Klucze są zawsze ciągami znaków, a wartości mogą być czymkolwiek: ciągami znaków, liczbami, tablicami, a nawet innymi obiektami.

```javascript
// Tworzenie tablicy asocjacyjnej
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Dostęp do elementów
console.log(userInfo.name); // Wynik: Alex
console.log(userInfo["email"]); // Wynik: alex@example.com

// Dodawanie nowych elementów
userInfo.job = "Developer";
userInfo["country"] = "Canada";

console.log(userInfo);
/* Wynik:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/

// Usuwanie elementu
delete userInfo.age;
console.log(userInfo);
/* Wynik:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/
```

Jak widać, dostęp, dodawanie lub usuwanie elementów w tablicy asocjacyjnej jest dość bezpośrednie i intuicyjne.

## Szczegółowo

W świecie JavaScript, choć często słyszymy termin "tablica asocjacyjna", jest to technicznie niewłaściwe określenie, ponieważ JavaScript nie posiada prawdziwych tablic asocjacyjnych jak inne języki (np. PHP). To, co ma JavaScript, to obiekty, które pełnią podobną funkcję, ale są bardziej potężną i elastyczną konstrukcją.

Historycznie, tablice w językach programowania były projektowane do przechowywania kolekcji przedmiotów, dostępnych poprzez ich numeryczny indeks. Jednak w miarę ewolucji rozwoju oprogramowania pojawiła się potrzeba bardziej elastycznych struktur danych. Tablice asocjacyjne, czy słowniki w innych językach, były jedną z odpowiedzi, umożliwiając dostęp do elementów przez dowolne klucze.

Podejście JavaScript z użyciem obiektów jako magazynów klucz-wartość oferuje mieszankę funkcjonalności. Umożliwia dodawanie, usuwanie i wyszukiwanie właściwości (kluczy) po nazwie. JSON (JavaScript Object Notation) jest świadectwem użyteczności tej struktury, stając się de facto standardem wymiany danych w sieci.

Chociaż obiekty pokrywają większość potrzeb dla tablic asocjacyjnych, w przypadkach, gdy ważna jest kolejność kluczy lub iteracja, obiekt `Map` wprowadzony w ES6 stanowi lepszą alternatywę. `Map` zachowuje kolejność kluczy, akceptuje szerszy zakres typów danych jako klucze i zawiera przydatne metody do iteracji i uzyskiwania rozmiaru. Mimo tych zalet, tradycyjna składnia obiektu pozostaje popularna ze względu na swoją prostotę i łatwość użycia w wielu typowych scenariuszach.
