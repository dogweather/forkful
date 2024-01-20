---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków pasujących do wzorca polega na usunięciu ze ciągu znaków wszystkich symboli, które pasują do określonego wzorca. Programiści robią to, aby zmanipulować dane tekstowe - czy to do czyszczenia danych, czy do ułatwienia analizy tekstu.

## Jak to zrobic:
Możemy to zrobić w JavaScript za pomocą metody `replace()`. Oto prosty przykład:

```Javascript
let str = "Cześć, jak się masz?";
let newStr = str.replace(/Cześć/g, '');
console.log(newStr);
```

Po uruchomieniu powyższego kodu, dostaniesz:

```
, jak się masz?
```

## Głębsze zanurzenie
Usuwanie znaków pasujących do wzorca nie jest nowym koncepcją. Zostało to wprowadzone w JavaScript 1.2, co jest starą wersją tego języka. 

Alternatywą dla metody `replace()` jest użycie wyrażeń regularnych. Wyrażenia regularne są bardziej elastyczne i potężne, choć są nieco trudniejsze do zrozumienia.

Ważny szczegół implementacyjny: Metoda `replace()` nie zmienia oryginalnego łańcucha. Tworzy nowy łańcuch ze zmienionymi danymi.

## Zobacz również
1. [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2. [Wyrażeń regularne w JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)