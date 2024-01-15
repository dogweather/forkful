---
title:                "Zmiana wielkości liter w ciągu znaków"
html_title:           "TypeScript: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, w których konieczne jest zmiana wielkości liter w łańcuchu znaków. Może to być potrzebne, aby odpowiednio sformatować wyświetlane dane lub zapewnić spójność w bazie danych. Bez względu na przyczynę, TypeScript oferuje prosty sposób na kapitalizację łańcucha znaków, który może zaoszczędzić czas i zapobiec błędom w pisowni.

## Jak to zrobić

Aby skorzystać ze wbudowanej metody TypeScript do kapitalizacji łańcucha znaków, wykonaj następujące kroki:

```TypeScript
const myString = 'witaj świecie';
const capitalizedString = myString.toUpperCase();

console.log(capitalizedString);

// Output: WITAJ ŚWIECIE
```

W powyższym przykładzie, deklarujemy zmienną `myString` z wartością "witaj świecie". Następnie, wykorzystując metodę `toUpperCase()`, tworzymy nową zmienną `capitalizedString`, która przechowuje skapitalizowany łańcuch znaków. W końcu, wyświetlamy wartość nowej zmiennej w konsoli, aby sprawdzić, czy została poprawnie zmieniona.

Może się zdarzyć, że chcesz zmienić na wielką literę tylko pierwszą literę w łańcuchu znaków, zamiast całego łańcucha. W takiej sytuacji możesz skorzystać z metody `charAt()` do pobrania pierwszego znaku, a następnie użyć metody `toUpperCase()` na nim i dokleić resztę łańcucha znaków.

```TypeScript
const myString = 'witaj świecie';
const capitalizedString = myString.charAt(0).toUpperCase() + myString.slice(1);

console.log(capitalizedString);

// Output: Witaj świecie
```

W tym przykładzie, wykorzystujemy metodę `charAt()` do pobrania pierwszego znaku z łańcucha znaków i używamy metody `toUpperCase()` na nim, aby zmienić na wielką literę. Następnie, wykorzystujemy metodę `slice()` do pobrania reszty łańcucha od drugiej litery i doklejamy ją do wczesniej wygenerowanej wielkiej litery.

## Wnikliwe spojrzenie

Metoda `toUpperCase()` jest wbudowanym elementem typu `string` w TypeScript. Oznacza to, że jest dostępna dla wszystkich zmiennych typu `string`, które możesz zadeklarować w swoim kodzie. Jest to bardzo przydatna opcja, która może zaoszczędzić czas i wysiłek w wielu sytuacjach.

See Also:

- [Dokumentacja TypeScript - Metoda toUppercase()](https://www.typescriptlang.org/docs/handbook/2/objects.html#methods)
- [Dokumentacja JavaScript - Metoda toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Tutorial z metodami ciągów](https://www.typescriptlang.org/docs/handbook/2/objects.html#creation-with)