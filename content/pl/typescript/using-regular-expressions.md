---
title:    "TypeScript: Używanie wyrażeń regularnych"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego?

Regularne wyrażenia to potężne narzędzie w programowaniu, pozwalające na efektywne manipulowanie i przetwarzanie tekstów. Jest to niezbędne w przypadku projektów wymagających analizy i wstępnej obróbki danych, optymalizacji algorytmów czy walidacji formularzy. Niezależnie od tego, czy jesteś początkującym programistą, czy doświadczonym weteranem, warto poznać możliwości i zastosowania regularnych wyrażeń.

## Jak to zrobić?

Aby rozpocząć pracę z regularnymi wyrażeniami w TypeScript, musimy najpierw wykorzystać moduł wbudowany w język - `RegExp`. Możemy przekazać dwa argumenty: wzorzec, który chcemy znaleźć w tekście oraz opcjonalne flagi, które modyfikują sposób dopasowania. Wyrażenie regularne możemy zdefiniować za pomocą literałów lub konstruktora. Następnie możemy wykorzystać metody `match()` lub `test()` w celu odnalezienia dopasowań w tekście.

```TypeScript
let re1 = /abc/i; // wyrażenie szuka danych 'abc' z ignorowaniem wielkości liter
let re2 = new RegExp("def"); // wyrażenie szuka danych 'def' z uwzględnieniem wielkości liter

let str1 = "ABCabc";
let str2 = "defghi";

re1.test(str1); // true - wyrażenie zostaje odnalezione
re2.test(str2); // false - wyrażenie nie zostaje odnalezione
```

Jeśli chcemy uzyskać dokładne informacje o dopasowaniach, używamy metody `exec()`, która zwraca tablicę z dopasowanym tekstem oraz informacjami o grupach i indeksach. Możemy również wykorzystać metody `replace()` i `split()` do manipulacji tekstem na podstawie wyrażenia regularnego.

```TypeScript
let re = /(\w+)\s(\w+)/; // wyrażenie szuka wyrazów oddzielonych spacją

let str = "John Smith";

console.log(re.exec(str)); // ["John Smith", "John", "Smith"] - tablica z dopasowanym tekstem oraz grupami
console.log(str.replace(re, "$2, $1")); // "Smith, John" - zamiana kolejności wyrazów
console.log(str.split(re)); // ["John", "Smith"] - podział tekstu na tablicę na podstawie wyrażenia
```

## Deep Dive

Regularne wyrażenia to nie tylko proste dopasowywanie i manipulacja tekstem. Znajomość zaawansowanych technik może znacznie usprawnić naszą pracę i zwiększyć wydajność naszego kodu. Przykładem może być użycie operatora negacji `^` w ramach zbiorów znaków (`[]`). Dzięki niemu możemy wykluczyć pewne znaki z dopasowywania, co może być szczególnie przydatne przy walidacji danych. Możemy również wykorzystać wyrażenia regularne wraz z funkcjami wywołania zwrotnego (`callback`) i wyrażeniami funkcyjnymi w celu dynamicznego dopasowania i manipulacji tekstem.

## Zobacz też

- [Dokumentacja TypeScript - Wyrażenia regularne](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutorial z wykorzystaniem wyrażeń regularnych w TypeScript](https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm)
- [Narzędzie do tworzenia i testowania wyrażeń regularnych](https://regex101.com/)