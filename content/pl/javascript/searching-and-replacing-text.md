---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wyszukiwanie i zastępowanie tekstu to podstawowe operacje manipulacji tekstem w dowolnym języku programowania. Programiści robią to na co dzień, aby zmieniać, aktualizować, ulepszać lub naprawiać dane tekstowe we własnym kodzie.

## Jak to zrobić:

W JavaScript możesz użyć wbudowanej metody `replace()`, aby zlokalizować i zastąpić tekst. Na przykład:

```Javascript
let string = "Cześć, jestem Programista";
let newString = string.replace("Programista", "JavaScript Dev");
console.log(newString);  // Wypisze: "Cześć, jestem JavaScript Dev"
```
W powyższym przykładzie, metoda `replace()` przeszukuje wartość zmiennej `string` w poszukiwaniu słowa "Programista" i zastępuje go tekstem "JavaScript Dev".

## Głębsza analiza:

Metoda `replace()` w JavaScript ma swoje korzenie w historycznym kontekście języków programowania. Pochodzi z tradycji wcześniejszych języków, takich jak PERL, gdzie strumień tekstu był przetwarzany linia po linii, a każde wystąpienie wzorca było zastępowane na bieżąco.

Co do alternatyw, JavaScript oferuje `RegExp` (Regular Expressions). "RegExp" znajduje szersze użycie, gdy mamy wiele wystąpień do zastąpienia. Metoda `replace()` zastępuje tylko pierwsze dopasowanie, chyba że użyjesz wyrażeń regularnych. 

```Javascript
let str = "Jak jesteś? Jak się masz?";
let newStr = str.replace(/Jak/g, "Dzień dobry");
console.log(newStr);  // Wypisze: "Dzień dobry jesteś? Dzień dobry się masz?"
```
Powód, dla którego funkcja ta nie zastępuje wszystkich wystąpień bez RegExp, wynika z zasady jednokrotnego dopasowania, która jest domyślna dla wielu operacji na łańcuchach w większości języków programowania.

## Zobacz także:

Dodatkowe źródła do nauki i zrozumienia szukania i zastępowania tekstu w JavaScript:
1. W3Schools: [JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
2. MDN Web Docs: [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
3. JavaScript.INFO: [Strings](https://javascript.info/string)
4. GeeksforGeeks: [JavaScript | String replace()](https://www.geeksforgeeks.org/javascript-string-replace/)