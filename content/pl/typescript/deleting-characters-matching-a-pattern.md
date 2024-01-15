---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "TypeScript: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów może znaleźć się w sytuacji, gdzie muszą usunąć pewne znaki z ciągu znaków, które spełniają określony wzorzec. Może to wynikać z potrzeby filtrowania danych lub zmiany formatu tekstu. W takich przypadkach znajomość sposobów na usuwanie znaków zgodnych z wzorcem przydaje się i może znacznie ułatwić pracę.

## Jak to zrobić

W TypeScript istnieje wiele sposobów na usunięcie znaków dopasowujących się do wzorca. Jednym z najprostszych może być użycie funkcji `replace()` i wykorzystanie wyrażenia regularnego w celu określenia wzorca. Na przykład:

```TypeScript
const stringToFilter = "12aa34BB56cd";

console.log(stringToFilter.replace(/[a-zA-Z]/g, ""));

// wynik: 123456
```

Możemy również wykorzystać metodę `filter()` dla tablic, aby usunąć znaki spełniające określony wzorzec. Przykład:

```TypeScript
const arrayToFilter = ["apple", "banana", "123", "456", "cherry"];

const filteredArray = arrayToFilter.filter(item => !/^[a-z]+$/i.test(item));

console.log(filteredArray);

// wynik: ["123", "456"]
```

## Wnikliwe omówienie

Istnieje wiele zaawansowanych metod usuwania znaków dopasowujących się do wzorca w TypeScript, takich jak użycie modułu `string-matching` lub wykorzystanie gotowych funkcji biblioteki `lodash`. Istnieje również możliwość wykorzystania wyrażeń regularnych z parametrem globalnym `g` lub wykorzystanie metod `slice()` lub `substr()` dla obiektów typu `string`. Warto więc mieć świadomość różnych sposobów na filtrowanie danych i wybrać ten najlepiej odpowiadający potrzebom projektu.

## Zobacz również

- [Dokumentacja TypeScript na temat wyrażeń regularnych](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Artykuł na temat wykorzystania metod `slice()` i `substr()`](https://www.oreilly.com/library/view/javascript-the-definitive/0596000480/ch04s09.html)
- [Książka "Effective TypeScript" zawierająca wiele przykładów usuwania znaków dopasowujących się do wzorca](https://effective-typescript.com/2020/02/06/remove-falsy/)