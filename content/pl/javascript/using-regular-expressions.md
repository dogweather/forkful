---
title:                "Użycie wyrażeń regularnych"
html_title:           "Javascript: Użycie wyrażeń regularnych"
simple_title:         "Użycie wyrażeń regularnych"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robić?
Wykorzystanie wyrażeń regularnych w programowaniu jest bardzo powszechne. Są to specjalne wzorce lub wyrażenia, które pozwalają na wykonywanie operacji na ciągach tekstowych w sposób bardziej złożony i elastyczny. Programiści używają wyrażeń regularnych, aby skanować, porównywać, wyciągać lub modyfikować dane w dokumentach tekstowych, logach, bazach danych i wielu innych miejscach.

## Jak to zrobić:
W poniższych przykładach wykorzystamy funkcję test() do sprawdzania dopasowania wyrażeń regularnych w tekście. Aby rozpocząć, musimy stworzyć wzorzec, który zostanie porównany z tekstem. Wzorce w wyrażeniach regularnych są otoczone ukośnikami ```/``` i mogą zawierać litery, cyfry, znaki specjalne i wiele innych. Na przykład, aby znaleźć wszystkie wystąpienia słowa "hello" w tekście, użyjemy wzorca ```/hello/```, a następnie użyjemy funkcji test() aby sprawdzić, czy wzorzec pasuje do tekstu.
```Javascript
//Przykład 1
var text1 = "Hello World!";
var pattern1 = /Hello/;
console.log(pattern1.test(text1));//true

//Przykład 2
var text2 = "Hi there!";
console.log(pattern1.test(text2));//false
```
## Głębsze wgrzebanie:
Wyrażenia regularne zostały opatentowane w 1957 roku przez Stephena Cole'a Kleene'a. Od tego czasu wyrażenia regularne przeszły długą drogę i są obecnie obsługiwane przez wiele języków programowania, w tym przez JavaScript. Alternatywne sposoby na wykonywanie operacji na tekście to m.in. pętle lub wbudowane metody dla stringów, ale wyrażenia regularne pozwalają na wykonywanie bardziej skomplikowanych zadań, takich jak wyszukiwanie wzorców, które nie są dokładnie określone.

## Zobacz również:
Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych w JavaScript, polecam przeczytać dokumentację na MDN: https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions