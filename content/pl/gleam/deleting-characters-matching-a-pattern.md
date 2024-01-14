---
title:    "Gleam: Usuwanie znaków odpowiadających wzorcowi"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków pasujących do określonego wzorca może być bardzo przydatną umiejętnością w programowaniu. Czasami chcemy pozbyć się niepotrzebnych znaków z tekstu lub zastąpić je innymi. W tym artykule dowiesz się, jak wykonać tę operację w języku programowania Gleam.

## Jak to zrobić

Aby w prosty sposób usunąć znaki pasujące do wzorca, możemy skorzystać z funkcji `String.replace` w języku Gleam. Przyjmujemy dwa argumenty: pierwszy to wzorzec, który chcemy usunąć, a drugi to tekst, w którym chcemy dokonać zmiany. Następnie wywołujemy tę funkcję wewnątrz kodu `let` i przypisujemy wynik do nowej zmiennej.

Przykładowy kod wykorzystujący tę funkcję:

```
Gleam
let text = "To jest przykładowy tekst do zmodyfikowania."
let modified_text = String.replace("przykładowy", "", text)
```

W powyższym przykładzie zastępujemy słowo "przykładowy" pustym ciągiem znaków, co oznacza jego usunięcie. Możemy oczywiście zamiast pustego ciągu podać również inny wzorzec, który ma zastąpić usunięte znaki.

## Deep Dive

Technika usuwania znaków pasujących do wzorca może okazać się bardzo przydatna w różnych sytuacjach. Możemy wykorzystać ją na przykład do oczyszczania danych wejściowych lub do edycji tekstów w naszych aplikacjach.

W języku Gleam istnieje również wiele innych przydatnych funkcji do operacji na tekstach, takich jak `String.split`, `String.trim` czy `String.to_uppercase`, które mogą również zostać wykorzystane w połączeniu z funkcją `String.replace`.

## Zobacz również

- Dokumentacja funkcji `String.replace`: [link](https://gleam.run/documentation/std/string#replace)
- Inne przydatne funkcje do operacji na tekstach w języku Gleam: [link](https://gleam.run/documentation/std/string)