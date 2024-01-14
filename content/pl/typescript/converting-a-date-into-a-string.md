---
title:    "TypeScript: Przekształcenie daty w łańcuch znaków"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty do formatu tekstowego jest częstym wyzwaniem podczas programowania z wykorzystaniem języka TypeScript. Jest to ważna umiejętność, ponieważ często musimy wyświetlać daty w formacie zrozumiałym dla użytkownika. W tym blogu dowiesz się, dlaczego warto umieć konwertować datę na string oraz jak to zrobić w języku TypeScript.

## Jak to zrobić

Aby skonwertować datę na string w języku TypeScript, możemy skorzystać z funkcji `toString()` lub `toLocaleDateString()`. Poniżej przedstawiamy przykładowy kod oraz jego wynik w dwóch różnych formatach daty:

```TypeScript
// Przykładowa data
const date = new Date("2021-04-15");

// Skonwertowanie daty na string przy użyciu metody toString()
const dateString1 = date.toString();
console.log(dateString1); // Output: Thu Apr 15 2021 00:00:00 GMT+0200 (Central European Summer Time)

// Skonwertowanie daty na string przy użyciu metody toLocaleDateString()
const dateString2 = date.toLocaleDateString();
console.log(dateString2); // Output: 15.04.2021
```

W powyższym przykładzie zobaczymy, że dla metody `toString()` domyślnym formatem jest format ISO, natomiast metoda `toLocaleDateString()` wyświetla datę w lokalnym formacie ustawionym na naszym komputerze.

## Deep Dive

Jednym z najważniejszych elementów konwertowania daty na string jest wybór odpowiedniego formatu. W języku TypeScript możemy skorzystać z funkcji `toLocaleDateString()` z dodatkowym parametrem `options`, w którym możemy precyzyjnie określić, jaki format daty nas interesuje.

```TypeScript
// Przykładowa data
const date = new Date("2021-04-15");

// Określenie formatu daty w parametrze options
const options = { year: 'numeric', month: 'long', day: 'numeric' };

// Konwersja daty na string z określonym formatem
const dateString3 = date.toLocaleDateString(undefined, options);
console.log(dateString3); // Output: 15 kwietnia 2021
```

W powyższym przykładzie użyliśmy parametru `options` do ustalenia formatu daty w formacie miesiąca jako słowa i rok jako liczby. Dzięki temu mamy większą kontrolę nad wyświetlaną datą i możemy dostosować ją do naszych potrzeb.

## Zobacz także

- Dokumentacja języka TypeScript: https://www.typescriptlang.org/docs/
- Przewodnik po funkcjach daty i czasu w języku JavaScript: https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date
- Jak konwertować daty w języku TypeScript: https://www.educba.com/typescript-date-to-string/