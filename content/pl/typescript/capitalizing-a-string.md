---
title:                "TypeScript: Zmiana pierwszej litery na wielką w ciągu znaków"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli jesteś programistą lub uczysz się TypeScript, na pewno spotkasz się z sytuacją, w której będziesz musiał zmienić wielkość liter w jakimś tekście. W tym artykule dowiesz się, dlaczego jest to ważne i jak możesz to zrobić w TypeScript.

## Jak to zrobić?

Sprawdzanie, czy jesteś programistą, często wiąże się z pisaniem kodu, który manipuluje tekstem. Ta umiejętność jest szczególnie przydatna, gdy na przykład otrzymujesz dane od użytkownika i musisz upewnić się, że są one w odpowiednim formacie.

Do zmienienia wielkości liter w tekście w TypeScript możesz użyć metody `toUpperCase()`. Przykładowy kod wygląda następująco:

```TypeScript
let tekst = "witaj świecie!";
console.log(tekst.toUpperCase());
```

Po uruchomieniu tego kodu, powinieneś zobaczyć następujący wynik w konsoli:

```
WITAJ ŚWIECIE!
```

## Deep Dive

To, co właśnie zobaczyłeś, jest tak naprawdę wywołaniem metody `toUpperCase()`. Jest to wbudowana metoda w JavaScript, z której możemy również skorzystać w TypeScript, ponieważ TypeScript jest nadzbiorem języka JavaScript.

Jedną z cech tej metody jest to, że zwraca ona nowy ciąg tekstowy, a nie modyfikuje oryginalnego. Oznacza to, że możesz przydzielić zmienną z wynikowym tekstem do nowej zmiennej i używać jej w innych częściach kodu.

Niektóre inne przydatne metody do zmiany wielkości liter to `toLowerCase()` i `toLocaleUpperCase()`. Pierwsza z nich zamienia wszystkie litery na małe, a druga uwzględnia lokalizację i zwraca wielkość liter odpowiadającą ustawieniom językowym użytkownika.

## Zobacz również

- [Dokumentacja TypeScript - Metody string](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html)
- [W3Schools - Metody string w TypeScript](https://www.w3schools.com/js/js_string_methods.asp)
- [Mozilla Developer Network - Metody string w JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/prototype)