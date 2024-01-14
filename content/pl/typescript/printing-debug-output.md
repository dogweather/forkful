---
title:    "TypeScript: Wyświetlanie danych diagnostycznych"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wydaje się, że drukowanie informacji debugujących jest niepotrzebne, jednak może być bardzo przydatne w procesie tworzenia oprogramowania. Pozwala na wczesne wykrywanie błędów oraz ułatwia debugowanie kodu. Dlatego warto poznać tę technikę i nauczyć się jej używać.

## Jak to zrobić

Aby wydrukować informacje debugujące w języku TypeScript, można skorzystać z funkcji `console.log()` lub `console.debug()`. Na przykład:

```TypeScript
let number = 10;
console.log("Wartość zmiennej number to: " + number);
```

Powyższy kod spowoduje wydrukowanie w konsoli wartości zmiennej `number`, czyli w tym przypadku będzie to `10`.

Można również drukować więcej informacji w jednym wierszu, korzystając ze symbolu `%`, np.:

```TypeScript
let firstName = "Anna";
let lastName = "Kowalska";
console.debug("Imię: %s, Nazwisko: %s", firstName, lastName);
```

Powyższy kod spowoduje wydrukowanie `Imię: Anna, Nazwisko: Kowalska` w konsoli.

Inną przydatną funkcją jest `console.table()`, która pozwala na wydrukowanie zawartości tablic w czytelnej formie tabeli, np.:

```TypeScript
let fruits = ["Apple", "Banana", "Orange"];
console.table(fruits);
```

Powyższy kod spowoduje wydrukowanie tabeli z trzema kolumnami: `Object`, `0` i `Apple`, `1` i `Banana`, `2` i `Orange`.

## Głębszy zanurzenie

Poza funkcjami `console.log()` i `console.debug()`, istnieje wiele innych przydatnych metod w obiekcie `console`. Niektóre z nich to `console.error()` do drukowania informacji o błędach, `console.info()` do wyświetlania informacji, `console.warn()` do ostrzegania przed potencjalnymi problemami oraz `console.group()` do grupowania informacji.

Warto również wspomnieć o `console.trace()`, która pozwala na śledzenie kolejnych wywołań funkcji, co może być bardzo pomocne w analizie błędów.

## Zobacz także

- [Dokumentacja funkcji console w języku TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html)
- [Przykłady wykorzystania informacji debugujących w TypeScript](https://www.digitalocean.com/community/tutorials/how-to-debug-typescript-with-vscode)

Dzięki wykorzystywaniu informacji debugujących w języku TypeScript, możliwe jest szybsze i bardziej efektywne debugowanie kodu oraz uniknięcie wielu błędów. Polecam wypróbować tę technikę w swoim kolejnym projekcie.