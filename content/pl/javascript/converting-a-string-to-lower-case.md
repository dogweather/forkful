---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Javascript: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie tekstu na małe litery jest powszechnym zadaniem w programowaniu Javascript. Jest to przydatne, gdy chcesz porównać dwa teksty niezależnie od wielkości liter lub wyświetlić tekst w jednolitej formie.

## Jak to zrobić

Konwersja tekstu na małe litery jest bardzo prosta w Javascript. Możesz użyć metody ```toLowerCase()```, która jest wbudowana w obiekcie String. Poniżej jest przykładowy kod, który konwertuje napis "JAkiSz TEkSt" na "jakisz tekst" i wyświetla go w konsoli.

```Javascript
let tekst = "JAkiSz TEkSt";
console.log(tekst.toLowerCase());
```

Output:
```
jakisz tekst
```

Jeśli chcesz zmienić wartość zmiennej tekstowej na zawsze, możesz przypisać nową wartość do zmiennej:
```Javascript
tekst = tekst.toLowerCase();
console.log(tekst);
```

Output:
```
jakisz tekst
```

## Głębsze wgląd

Metoda ```toLowerCase()``` jest jedną z wielu wbudowanych funkcji, które pomagają w manipulowaniu tekstami w Javascript. Ta metoda po prostu przekształca wszystkie duże litery w napisie na małe litery.

W Javascript istnieją również inne sposoby na konwertowanie tekstu na małe litery, takie jak metoda ```replace()```, która może zastąpić określone fragmenty tekstu i ```concat()```, która łączy dwa lub więcej ciągów znakowych. Możesz również użyć funkcji pętli, aby iteracyjnie zmienić każdą literę w napisie na małe litery.

## Zobacz także

1. Dokumentacja metody ```toLowerCase()```: https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/toLowerCase
2. Wideo na YouTube o konwertowaniu tekstu na małe litery w Javascript: https://www.youtube.com/watch?v=InfpmdOt3ik
3. Praktyczne zastosowania konwersji tekstu na małe litery: https://www.geeksforgeeks.org/javascript-program-to-convert-string-into-lower-case/