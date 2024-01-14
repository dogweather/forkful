---
title:                "Javascript: Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego warto usuwać znaki pasujące do wzorca

Czasami, podczas tworzenia programów, możemy natknąć się na sytuację, w której chcemy usunąć konkretne znaki z łańcucha tekstowego. Może to być potrzebne, na przykład, podczas filtrowania danych lub sprawdzania walidacji. W takich przypadkach, usunięcie znaków pasujących do wzorca może znacznie ułatwić nam pracę i zwiększyć wydajność naszego kodu.

## Jak to zrobić

Najbardziej efektywnym sposobem na usunięcie znaków pasujących do wzorca jest użycie metody `replace()` w połączeniu z wyrażeniem regularnym. Załóżmy, że chcemy usunąć wszystkie spacje z łańcucha tekstowego. W tym przypadku, wyrażeniem regularnym będzie `/ /g` (spacja między ukośnikami oznacza wystąpienie pojedynczej spacji, a flaga `g` oznacza globalne wyszukiwanie). Następnie, wywołując metodę `replace()` na naszym łańcuchu tekstowym, przekazujemy jako pierwszy argument nasze wyrażenie regularne, a jako drugi pusty łańcuch tekstowy. To spowoduje usunięcie wszystkich spacji z tekstu.

```Javascript
let text = "To jest przykładowy tekst.";
let pattern = / /g;
let result = text.replace(pattern, "");
console.log(result); // Output: Tojestprzykładowytekst.
```

## Głębszy zanurzenie

Wyrażenia regularne są potężnym narzędziem w programowaniu, pozwalającym na precyzyjne manipulowanie tekstami. W przypadku usuwania znaków pasujących do wzorca, przydatne mogą być różne symbole, jak na przykład `+` (oznacza powtórzenia), `^` (oznacza negację) czy `[]` (oznacza grupę znaków do dopasowania). Możliwości jest wiele, dlatego warto przejrzeć dokumentację i eksperymentować z wyrażeniami regularnymi, aby lepiej zrozumieć ich działanie.

## Zobacz także

- [Dokumentacja wyrażeń regularnych w Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Tutorial o wyrażeniach regularnych w Javascript](https://www.youtube.com/watch?v=95sQiSyIWHI)
- [Kurs o podstawach wyrażeń regularnych w Javascript](https://www.codecademy.com/learn/introduction-to-javascript)