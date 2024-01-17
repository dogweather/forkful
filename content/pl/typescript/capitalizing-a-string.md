---
title:                "Zapisywanie napisu z dużymi literami"
html_title:           "TypeScript: Zapisywanie napisu z dużymi literami"
simple_title:         "Zapisywanie napisu z dużymi literami"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co to jest oraz dlaczego? 
W programowaniu, zazwyczaj chcemy, aby tekst był czytelny i spójny. Jednym ze sposobów osiągnięcia tego jest poprawne kapitalizowanie wyrazów, czyli zamiana pierwszej litery w słowie na dużą. Programiści często stosują to w celu lepszej czytelności kodu oraz estetycznego wyglądu.

## Jak to zrobić: 
W języku TypeScript, możemy użyć wbudowanej metody ```toUpperCase()``` do kapitalizowania stringów. Przykładowe użycie wyglądałoby następująco:

```TypeScript
let word = "programowanie";
let capitalizedWord = word.charAt(0).toUpperCase() + word.slice(1); 

console.log(capitalizedWord);
// Output: Programowanie 
```

## Głębszy wygląd: 
Praktyka kapitalizowania stringów wywodzi się z tradycji pisania tekstu w języku angielskim, gdzie wielka litera jest używana na początku każdego zdania lub wyrazu w nagłówkach i tytułach. W języku TypeScript ta funkcjonalność jest wbudowana, ale istnieją również inne sposoby na kapitalizowanie stringów, takie jak korzystanie z biblioteki **lodash**, która oferuje wiele przydatnych funkcji tekstowych, w tym ```capitalize()```.

## Zobacz również: 
- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
- [Metoda toUpperCase() w JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Biblioteka lodash](https://lodash.com/)