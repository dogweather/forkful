---
title:                "Javascript: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu może wystąpić potrzeba usunięcia znaków zgodnie z pewnym wzorcem. To może być konieczne podczas pracy z tekstem lub w celu oceny danych wejściowych. W tym wpisie dowiesz się, jak łatwo zaimplementować taką funkcjonalność w języku JavaScript.

## Jak to zrobić

W celu usunięcia wszystkich znaków zgodnie z pewnym wzorcem, użyjemy funkcji `replace()` i wykorzystamy wyrażenie regularne. Przykładowy kod dla tego zadania wyglądałby następująco:

```Javascript
let string = "Ala ma kota 123";
let newString = string.replace(/[a-z0-9]/gi, "");

console.log(newString); // W tym przykładzie powinno wyświetlić się: "  "
```

Powyższy przykład pokazuje, że dzieki wyrażeniu regularnemu `[a-z0-9]` wszystkie litery oraz cyfry zostaną usunięte z tekstu. Jeśli chcemy usunąć tylko litery, możemy użyć wyrażenia `[a-z]`.

 ## Głębsza analiza

Funkcja `replace()` wykonuje jedynie pierwsze dopasowanie, dlatego jeśli chcielibyśmy usunąć wszystkie znaki zgodnie z wzorcem, musimy dodać flagę `g` (global) do wyrażenia regularnego. Jest to szczególnie przydatne, gdy posiadamy więcej niż jedno wystąpienie znaków, które chcemy usunąć.

Dodatkowo, poprzez użycie flagi `i` (case-insensitive), możemy zignorować wielkość liter w tekście. Zatem jeśli naszym wzorcem jest jedynie `a`, a chcemy usunąć wszystkie wystąpienia zarówno litery `A` jak i `a`, użyjemy wyrażenia `[a]` oraz flagi `i`.

Kolejnym przydatnym sposobem manipulacji tekstem jest wykorzystanie funkcji `split()` oraz `join()`. Pierwsza służy do podziału tekstu na tablicę, a druga do ponownego połączenia tej tablicy w jeden ciąg znaków. Przykładowy kod wyglądałby następująco:

```Javascript
let string = "Ala ma kota 123";
let newString = string.split("").filter(char => char !== " ").join("");

console.log(newString); // W tym przykładzie także wyświetli się: "Alamakota123"
```

W powyższym przykładzie, dodatkowo używamy funkcji `filter()` w celu usunięcia spacji z tekstu. Dzięki temu, możemy kontrolować, jakie znaki zostaną usunięte zgodnie z naszym wzorcem.

## Zobacz również

- [Dokumentacja funkcji replace() na MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Wprowadzenie do wyrażeń regularnych w JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Dokumentacja funkcji split() na MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [Dokumentacja funkcji join() na MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)