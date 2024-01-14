---
title:                "Javascript: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z ważnych umiejętności w programowaniu jest łączenie, zwane również konkatencją, różnych ciągów znaków w jeden. W JavaScript, istnieje kilka sposobów, aby to osiągnąć i jest używana w wielu przypadkach, aby zmienić wygląd lub treść na stronie internetowej.

## Jak To Zrobić

Istnieje kilka sposobów, aby połączyć ciągi znaków w JavaScript. Można tego dokonać za pomocą operatora "+" lub metody "concat()". Poniżej przedstawione są przykładowe fragmenty kodu wraz ze wzorcowym wyjściem dla obu metod:

```Javascript
// Użycie operatora "+" 
let imie = "Anna";
let nazwisko = "Kowalska";
let pelneDane = imie + " " + nazwisko;
console.log(pelneDane);
// Output: "Anna Kowalska" 

// Użycie metody "concat()" 
let pierwszeImie = "Julia";
let drugieImie = "Maria";
let zakonczenia = "i Barbara";
let calaNazwa = pierwszeImie.concat(" ", drugieImie, " ", zakonczenia);
console.log(calaNazwa);
// Output: "Julia Maria i Barbara"
```

Użycie operatora "+" jest prostsze i czytelniejsze, jednak metoda "concat()" jest bardziej wydajna dla łączenia wielu ciągów znaków.

## Dogłębna Analiza

W JavaScript, łączenie ciągów znaków jest możliwe dzięki konwersji ciągów na typ danych "String". W przypadku użycia operatora "+", każdy operand jest przekształcany na string i następnie są łączone. Natomiast metoda "concat()" przyjmuje wiele argumentów, które są następnie dodawane do pierwotnego ciągu. Jedną z zalet tej metody jest możliwość zastosowania jej w połączeniu z zmiennymi.

Jednak przy łączeniu dużej ilości ciągów znaków, może być wydajniejsze użycie operatora "+". Jest to spowodowane faktem, że każde wywołanie metody "concat()" powoduje utworzenie nowego obiektu typu "String", co może być kosztowne dla pamięci i wydajności.

## Zobacz również

- [W3Schools.com - JavaScript String Concat() Method](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [MDN Web Docs - String.prototype.concat()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [FreeCodeCamp.org - How to Concatenate Strings in JavaScript](https://www.freecodecamp.org/news/how-to-concatenate-strings-in-javascript/)