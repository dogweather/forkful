---
title:    "Javascript: Używanie wyrażeń regularnych"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego?

 Programowanie może wydawać się trudne i skomplikowane dla wielu ludzi, jednak znajomość regularnych wyrażeń może znacznie ułatwić pracę z kodem. Regularne wyrażenia to zestaw reguł, które pozwalają na wyrażenie i przetwarzanie danych w sposób bardziej precyzyjny. Dzięki nim możemy szybko i skutecznie przeszukiwać tekst, wybierać konkretne fragmenty lub dokonywać zmian w naszym kodzie.

## Jak to zrobić?

Aby zacząć korzystać z regularnych wyrażeń w Javascript, wystarczy wykorzystać wbudowany obiekt `RegExp`. Na początku definiujemy wzór, który chcemy przetestować lub wykorzystać do przetwarzania danych. Następnie używamy odpowiedniej funkcji dla naszych potrzeb, np. `test()` do sprawdzania czy dany łańcuch znaków pasuje do wzoru, lub `exec()` do pobrania informacji o dopasowaniu. Zobaczmy przykładowy kod:

```Javascript
// Definiujemy wzór do sprawdzenia
let wzorzec = /jestem/i;

// Przetestujemy czy dany łańcuch pasuje
console.log(wzorzec.test("Jestem programistą")); // Output: true
console.log(wzorzec.test("Cześć, jestem uczniem")); // Output: false

// Użyjemy exec() do pobrania informacji
console.log(wzorzec.exec("Jestem studentem").input); // Output: jestem studentem
console.log(wzorzec.exec("Hej, jestem studentką").input); // Output: null
```

## Dogłębny wgląd

Regularne wyrażenia mogą być używane w wielu różnych sytuacjach, na przykład do walidacji danych wejściowych, filtrowania danych lub wykonania pętli na wielu elementach. W Javascript możemy wykorzystywać różne flagi, aby uzyskać jeszcze bardziej precyzyjne dopasowanie lub wykonać działania np. bez uwzględniania wielkości liter. Ważne jest również zwrócenie uwagi na wyrażenia regularne globalne i nieglobalne, które mogą mieć różne zachowania przy wykonywaniu funkcji `test()` lub `exec()`.

## Zobacz również
- [Dokumentacja Javascript na temat regularnych wyrażeń](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Przewodnik po wyrażeniach regularnych w Javascript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Podstawy wyrażeń regularnych - kurs na platformie Codecademy](https://www.codecademy.com/learn/introduction-to-regular-expressions)