---
title:                "TypeScript: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery jest częstym zadaniem w programowaniu, szczególnie w tworzeniu aplikacji internetowych. Jest to ważne, ponieważ tablice znakowe są wrażliwe na wielkość liter, co może utrudnić porównywanie i sortowanie danych. Konwertowanie tekstu na małe litery pozwala na spójność i ułatwia pracę z tekstem w różnych językach.

## Jak to zrobić?

```TypeScript
let text: string = "Hello World!";
let lowerCase: string = text.toLowerCase();
console.log(lowerCase); // output: hello world!
```

Możemy użyć wbudowanej metody `toLowerCase()` na zmiennej tekstowej, aby przekonwertować ją na małe litery. Ta metoda zwraca nowy łańcuch znaków z małymi literami, a oryginalny tekst pozostaje bez zmian.

## Głębsza analiza

Konwertowanie tekstu na małe litery zależy od używanego języka programowania. W przypadku TypeScript, wbudowana metoda `toLowerCase()` wymaga wykorzystania klasy `String`, która jest obiektem globalnym. Metoda ta jest często wykorzystywana wraz z innymi funkcjami, takimi jak `toUpperCase()` czy `trim()`, co umożliwia dokładniejszą manipulację tekstem.

Jednym z ważnych aspektów konwertowania tekstu na małe litery jest obsługa znaków nielatynowskich. Na przykład, w językach takich jak turecki, niektóre litery mają swoją "małą wersję", która jest inna niż zwykła litera o mniejszej wielkości. W takim przypadku użycie metody `toLowerCase()` może spowodować nieoczekiwane wyniki, dlatego ważne jest zapoznanie się z wyjątkami w danym języku.

## Zobacz również

- [Dokumentacja TypeScript - Klasy String](https://www.typescriptlang.org/docs/handbook/strings.html)
- [JavaScript.info - String Methods](https://javascript.info/string-methods)
- [MDN Web Docs - String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)