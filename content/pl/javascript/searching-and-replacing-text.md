---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "Javascript: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastępowanie tekstu jest kluczowym elementem programowania, ponieważ pozwala nam w łatwy sposób wprowadzać zmiany w naszym kodzie. Jest to szczególnie przydatne, gdy chcemy wprowadzić jednocześnie wiele podobnych modyfikacji lub gdy potrzebujemy zmienić pewne elementy w wielu plikach jednocześnie.

## Jak to zrobić

```Javascript
// Przykładowy string, w którym chcemy dokonać zmiany
let string = "Cześć, jestem programistą w języku Javascript!"

// Użycie metody replace() do zamiany słowa "programistą" na "developerem"
let newString = string.replace("programistą", "developerem")

console.log(newString) 
// Output: Cześć, jestem developerem w języku Javascript!
```
W powyższym przykładzie użyliśmy metody `replace()` na stringu, aby zmienić jedno słowo na inne. Możemy w ten sam sposób wprowadzać zmiany w całych zdaniach lub wyrażeniach. Dodatkowo, możemy wykonać wielokrotne zastępowanie, używając wyrażenia regularnego oraz flagi globalnej `g`.

```Javascript
// Przykładowy string z wieloma wystąpieniami słowa "programista"
let string = "Jestem programistą zarówno w języku Javascript, jak i Python. Programista to bardzo ciekawy zawód."

// Użycie wyrażenia regularnego oraz flagi globalnej dla zastąpienia wszystkich wystąpień słowa "programista"
let newString = string.replace(/programista/g, "developer")

console.log(newString)
// Output: Jestem developerem zarówno w języku Javascript, jak i Python. Developer to bardzo ciekawy zawód.
```

## Głębszy przegląd

Podczas korzystania z metody `replace()` mamy również możliwość dostosowania naszego zastępowania za pomocą funkcji zwrotnej (callback). Możemy też używać wyrażeń regularnych z grupami dopasowań, aby wykonywać bardziej zaawansowane operacje zastępowania.

```Javascript
// Przykładowy string zawierający wielokrotne wystąpienie słów oddzielonych przecinkami
let string = "HTML, CSS, Javascript, Python, Java"

// Użycie funkcji zwrotnej, aby zamienić każde słowo na wersję z dużymi literami
let newString = string.replace(/\w+/g, function(match) {
  return match.toUpperCase()
})

console.log(newString)
// Output: HTML, CSS, JAVASCRIPT, PYTHON, JAVA

// Użycie grupy dopasowań, aby zamienić kolejność słów
let newString = string.replace(/(\w+),\s(\w+)/g, "$2, $1")

console.log(newString)
// Output: CSS, HTML, Python, Javascript, Java
```

## Zobacz też

- [MDN - Metoda replace()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/Replace)
- [W3Schools - Wyrażenia regularne w Javascript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [FreeCodeCamp - Przetwarzanie tekstu z Javascript](https://www.freecodecamp.org/news/how-to-process-text-with-javascript/)