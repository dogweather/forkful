---
title:    "Javascript: Wyszukiwanie i zamiana tekstu"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Zamienianie tekstu jest podstawowym zadaniem w programowaniu. Dzięki tej funkcji możemy łatwo i szybko zmieniać wybrane fragmenty tekstu, co jest niezbędne w wielu projektach.

## Jak to zrobić

Aby zamienić tekst w Javascript, możemy użyć kilku różnych sposobów. Jednym z najprostszych jest użycie funkcji `replace()`. Przykładowy kod wyglądałby tak:

```Javascript
let text = "Witaj na moim blogu!";
let newText = text.replace("Witaj", "Cześć");
console.log(newText);
```

Wynikiem tego kodu jest "Cześć na moim blogu!".

Możemy również użyć wyrażenia regularnego do zamiany tekstu. Przykładowy kod wyglądałby tak:

```Javascript
let text = "Zamiana wielu słów za pomocą wyrażeń regularnych.";
let newText = text.replace(/wielu słów/g, "dużo słów");
console.log(newText);
```

Wynikiem tego kodu jest "Zamiana dużo słów za pomocą wyrażeń regularnych." 

## Deep Dive

Funkcja `replace()` może przyjmować dwa parametry - wartość, którą chcemy zamienić oraz wartość, na którą chcemy ją zamienić. Możemy również przekazać funkcję jako drugi parametr, która będzie wywoływana na każdym dopasowanym fragmencie tekstu. Możemy w ten sposób dokonać bardziej zaawansowanych operacji podczas zamiany tekstu.

```Javascript
let text = "Zamiana wielu słów za pomocą funkcji zamieniającej.";
let newText = text.replace(/funkcji/g, function(match) {
  return "funkcję " + match.toUpperCase();
});
console.log(newText);
```

Wynikiem tego kodu jest "Zamiana wielu słów za pomocą funkcji ZAMIANIAJĄCEJ." 

## Zobacz również

* [Funkcja replace() w Javascript](https://www.w3schools.com/jsref/jsref_replace.asp)
* [Wyrażenia regularne w Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)