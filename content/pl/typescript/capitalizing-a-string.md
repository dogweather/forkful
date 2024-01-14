---
title:                "TypeScript: Zapisywanie napisu wielkimi literami"
simple_title:         "Zapisywanie napisu wielkimi literami"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że podczas pisania kodu potrzebujemy zmienić duże i małe litery w tekście. W takich przypadkach przydatna jest metoda "toUpperCase" dostępna w wielu językach programowania. W TypeScript można użyć tej metody aby zmienić wszystkie litery na duże w danym łańcuchu znaków.

## Jak To Zrobić

```TypeScript
let string = "to jest przykładowy tekst";
let capitalizedString = string.toUpperCase();

console.log(string); // wynik: to jest przykładowy tekst
console.log(capitalizedString); // wynik: TO JEST PRZYKŁADOWY TEKST
```

W powyższym przykładzie utworzyliśmy zmienną "string" i przypisaliśmy jej wartość "to jest przykładowy tekst". Następnie użyliśmy metody "toUpperCase" na zmiennej "string" i przypisaliśmy ją do nowej zmiennej "capitalizedString". W wyniku otrzymaliśmy zmienną "capitalizedString" zawierającą ten sam tekst, ale z wszystkimi literami zmienionymi na duże. Aby wyświetlić zawartość zmiennych w konsoli, użyliśmy metody "console.log".

## Deep Dive

Metoda "toUpperCase" jest dostępna dla wszystkich typów prostych zdefiniowanych w TypeScript, takich jak string, number czy boolean. Co więcej, można również użyć tej metody na całych wyrażeniach, a nie tylko pojedynczych zmiennych. Można również wykonywać konkatenację różnych ciągów znaków przy użyciu tej metody, co pozwala na jeszcze większą elastyczność w manipulowaniu tekstem.

## Zobacz Również

- [Dokumentacja TypeScript: String Operations](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string-operations)
- [W3Schools: TypeScript Strings](https://www.w3schools.com/ts/ts_strings.asp)
- [MDN: String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)