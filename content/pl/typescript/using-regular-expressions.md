---
title:                "Używanie wyrażeń regularnych"
html_title:           "TypeScript: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Używanie wyrażeń regularnych jest powszechne wśród programistów i służy do manipulacji i przetwarzania tekstu zgodnie z określonym wzorcem. Pozwala to na szybką i precyzyjną analizę tekstów oraz ułatwia pracę z danymi. Przydatne jest także w walidacji formularzy i filtrowaniu danych.

## Jak to zrobić:

Poniżej znajdują się przykłady kodu w języku TypeScript, które ilustrują działanie wyrażeń regularnych.

```TypeScript
let text = "Hello, World!";
let pattern = /^Hello/;
let result = pattern.test(text);
console.log(result); // true
```

W powyższym przykładzie, zmienna "result" zawiera wartość logiczną "true", ponieważ wyrażenie regularne sprawdza, czy tekst zaczyna się od słowa "Hello".

```TypeScript
let text = "I love TypeScript!";
let pattern = /[a-z]+/;
let result = pattern.exec(text);
console.log(result); // ["love"]
```

W tym przykładzie, wyrażenie regularne znajduje pierwsze wystąpienie ciągu znaków składającego się wyłącznie z małych liter i zwraca go jako rezultat.

## Głębsze zagębianie:

Wyrażenia regularne zostały stworzone w latach 50-tych przez matematyka Stephena Cole Kleene'a, a powszechnie wykorzystywane są w różnych językach programowania, m.in. JavaScript, Python czy Perl. Alternatywą dla wyrażeń regularnych jest użycie metod wbudowanych w język TypeScript, jednak zastosowanie wyrażeń regularnych jest zwykle bardziej wygodne i użyteczne w przypadku bardziej złożonych operacji.

W TypeScript, wyrażenia regularne są obiektami typu RegExp i wykorzystują metody takie jak "test" czy "exec". Dostępne są również specjalne znaki, tzw. metaznaki, które pozwalają na zdefiniowanie reguł dopasowania, np. "." oznacza dowolny znak, a "^" i "$" określają początek i koniec wyrażenia.

## Zobacz także:

- [Tutorial wyrażeń regularnych w JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Online edytor do wyrażeń regularnych](https://regex101.com/)