---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego? 
Konwersja łańcucha (stringa) na małe litery to proces zamiany wszystkich liter w danym łańcuchu na małe litery. Programiści robią to za każdym razem, gdy potrzebują upewnić się, że dane są spójne i niezależne, np. przy porównaniu łańcuchów.

## Jak to zrobić: 
Aby skonwertować łańcuch na małe litery w TypeScript, używamy metody `toLowerCase()`. Przyjmuje ona nie ma parametrów i zwraca nowy łańcuch, w którym wszystkie litery zostały zastąpione małymi literami.

```TypeScript
let tekst: string = "Hello, World!";
let tekstNaMaleLitery: string = tekst.toLowerCase();

console.log(tekstNaMaleLitery);  // "hello, world!"
```
Jak widać, metoda `toLowerCase()` zmieniła wszystkie litery na małe.

## Głębsze zanurzenie
Pierwsze wdrożenia konwersji znaków na małe litery datują się na lata 70. XX wieku z pojawieniem się pierwszych języków programowania wysokiego poziomu.
Alternatywnie można również użyć metody `toLocaleLowerCase()`, ktróra działa podobnie do `toLowerCase()`, ale jest bardziej przyjazna dla międzynarodowych znaków.

Jednakże, zarówno `toLowerCase()` jak i `toLocaleLowerCase()` nie zmieniają oryginalnego łańcucha. Tworzą one nowy łańcuch z literami przekształconymi na małe litery. Jest to spowodowane tym, że w JavaScript i TypeScript, łańcuchy są niezmienne.

```TypeScript
let tekst: string = "Hello, World!";
tekst.toLowerCase();

console.log(tekst);  // "Hello, World!"
```
Jak widać, mimo użycia metody `toLowerCase()`, oryginalny łańcuch pozostał niezmieniony.

## Zobacz również
[Agnostic functions - To Lower Case](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase) 
[MDN Web Docs - toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase) 
[Mozilla Hacks - A crash course in just-in-time (JIT) compilers](https://hacks.mozilla.org/2017/02/a-crash-course-in-just-in-time-jit-compilers/)