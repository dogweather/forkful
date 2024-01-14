---
title:    "TypeScript: Wycinanie podciągów"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystywanie substrings jest ważną umiejętnością w programowaniu, ponieważ pozwala nam wyodrębnić określone części tekstu lub ciągu znaków z większego zbioru danych. To nie tylko ułatwia czytanie i analizowanie danych, ale także może być przydatne w wielu różnych zastosowaniach, takich jak walidacja formularzy, manipulowanie tekstem i wiele innych.

## Jak to zrobić

Aby wyciąć substring w TypeScript, możemy użyć metody `substring()` lub `slice()`, w zależności od naszych potrzeb. Oba te metody przyjmują dwa parametry: początkowy i końcowy indeks, które określają początek i koniec fragmentu, który chcemy wyodrębnić.

```TypeScript
let string = "To jest tekst do przycięcia."
let substring1 = string.substring(3, 11) // rezultat: "jest tekst"
let substring2 = string.slice(16) // rezultat: "przycięcia."
```

W pierwszym przykładzie, używając `substring()`, wycięliśmy tekst zaczynający się od trzeciego indeksu (wliczając pierwszy znak jako 0) do jedenastego indeksu (bez niego). Natomiast w drugim przykładzie, zastosowaliśmy `slice()` z jednym parametrem, co spowodowało wycięcie fragmentu tekstu od szesnastego indeksu do końca.

## Głębsza analiza

Powyższe przykłady pokazują podstawowe użycie metody `substring()` i `slice()`, jednak warto wiedzieć, że istnieją również inne metody do wycinania substrings, takie jak `substr()` czy `substring-before()` w XPath. Każda z nich różni się szczegółami i może być przydatna w różnych sytuacjach. Dlatego warto poznać je wszystkie, aby wybrać najodpowiedniejszą w danym przypadku.

## Zobacz także

- Tutorial o używaniu substrings w TypeScript: [https://www.typescriptlang.org/docs/handbook/strings.html#substring-and-substr](https://www.typescriptlang.org/docs/handbook/strings.html#substring-and-substr)
- Przykładowe zadania i wyzwania z wykorzystaniem substrings: [https://www.freecodecamp.org/learn/javascript-algorithms-and-data-structures/basic-algorithm-scripting/slice-and-splice](https://www.freecodecamp.org/learn/javascript-algorithms-and-data-structures/basic-algorithm-scripting/slice-and-splice)
- Porównanie różnych metod wycinania substrings w programowaniu: [https://betterprogramming.pub/substring-substr-or-slice-how-to-substring-the-right-way-in-javascript-b6dfc365d1e4](https://betterprogramming.pub/substring-substr-or-slice-how-to-substring-the-right-way-in-javascript-b6dfc365d1e4)