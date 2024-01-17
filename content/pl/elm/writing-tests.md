---
title:                "Pisanie testów"
html_title:           "Elm: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-tests.md"
---

{{< edit_this_page >}}

"Czym jest i po co są testy?"

Testy pisane przez programistów są kodem, który sprawdza, czy dany program działa poprawnie. Jest to ważny krok w procesie tworzenia oprogramowania, ponieważ pomaga programistom upewnić się, że wszystko działa zgodnie z założeniami.

"Jak to zrobić?"

Kod testów w Elm jest napisany w taki sam sposób jak kod programu. Poniżej przedstawiam przykłady testów wraz z ich wynikami.

```Elm
test "Dodawanie liczb całkowitych" 
    (assert (1 + 1 == 2))
```

```Elm
test "Dodawanie liczb zmiennoprzecinkowych" 
    (assert (1.5 + 2.5 == 4.0))
```

```Elm
Dodawanie liczb całkowitych: PASS
Dodawanie liczb zmiennoprzecinkowych: PASS
```

"Dogłębna analiza"

Pisanie testów nie jest niczym nowym w świecie programowania. Już w latach 50. XX wieku programiści stosowali testy jednostkowe, czyli krótkie programy, które sprawdzały poprawność poszczególnych kawałków kodu. W Elm testy nazywają się "asercjami", a wykonują się podczas kompilacji programu.

Alternatywą dla pisania testów jest ręczne sprawdzanie poprawności kodu. Jednak taka metoda jest czasochłonna i podatna na błędy. Dzięki testom można szybko znaleźć i poprawić ewentualne problemy w programie.

Pisanie testów w Elm jest proste i nie wymaga specjalnej wiedzy ani narzędzi. Testy są zintegrowane z samym kodem programu i wykonują się automatycznie podczas jego kompilacji.

"Zobacz również"

- Dokumentacja Elm: https://elm-lang.org/docs
- Poradnik o pisaniu testów w Elm: https://medium.com/@thebucknellian/testing-in-elm-...690e3c3aebf
- Książka "Elm in Action" omawiająca testowanie kodu w Elm: https://www.manning.com/books/elm-in-action