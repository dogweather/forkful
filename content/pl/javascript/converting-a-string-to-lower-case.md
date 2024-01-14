---
title:                "Javascript: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, w których konwersja tekstu na małe litery jest pożądana. Na przykład, gdy porównujemy dwa wyrazy, ważne jest, aby nie uwzględniać wielkości liter, aby uzyskać dokładne wyniki. Ponadto, konwersja na małe litery jest często stosowana w celu lepszej czytelności i spójności w kodzie.

## Jak to zrobić?

Konwersja tekstu na małe litery jest bardzo prosta w języku JavaScript. Wystarczy użyć wbudowanej funkcji ```toLowerCase()``` i przekazać jako argument tekst, który chcemy przekonwertować. Na przykład:

```javascript
let text = "TEKST";
console.log(text.toLowerCase());
```

Wynik wyświetlony w konsoli będzie brzmiał "tekst". Jest to efektywny i szybki sposób na przekonwertowanie tekstu na małe litery.

## Głębszy wywód

W języku JavaScript, wszystkie ciągi znaków są traktowane jako obiekty. To oznacza, że ​​posiadają one metody i właściwości, takie jak ```toLowerCase()```, które mogą być wywoływane na nich. Jest to ciągłe ulepszanie języka JavaScript, które pozwala na bardziej wydajne i zwięzłe programowanie.

W przypadku konwersji tekstu na małe litery, funkcja ```toLowerCase()``` sprawdza każdy znak w ciągu i zamienia go na jego odpowiednik w postaci małej litery, zachowując przy tym inne znaki i znaki specjalne. Jest to bardzo przydatne narzędzie, które zapewnia dokładną konwersję bez uszkodzenia żadnych innych znaków.

## Zobacz również

- [Dokumentacja funkcji toLowerCase() w języku JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Inne przydatne funkcje do zarządzania tekstami w języku Javascript](https://www.tutorialspoint.com/what-are-the-string-functions-in-javascript)