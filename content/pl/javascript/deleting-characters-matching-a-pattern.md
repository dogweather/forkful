---
title:    "Javascript: Usuwanie znaków pasujących do wzorca"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego?

Czasami, podczas pisania kodu, możesz znaleźć się w sytuacji, w której musisz usunąć wszystkie znaki, które pasują do określonego wzoru. Może być to przydatne, gdy masz dużą ilość danych i chcesz usunąć niechciane znaki w celu poprawienia odczytu lub przetwarzania danych.

## Jak to zrobić?

W JavaScript, istnieje kilka sposobów na usunięcie znaków pasujących do wzoru. Jedną z metod jest użycie wyrażenia regularnego za pomocą funkcji `replace()`. W poniższym przykładzie, pokazuję, jak usunąć wszystkie liczby z ciągu znaków:

```Javascript
let str = "1a2b3c4d";
console.log(str.replace(/[0-9]/g, '')); // output: abcd
```

Jeśli chcesz usunąć wszystkie znaki specjalne, możesz użyć tego samego wyrażenia regularnego z dodanym znakiem ukośnika `\/` na początku, aby uniknąć konfliktu z meta-znakami. Przykładowy kod wyglądałby tak:

```Javascript
let str = "a!b@c#d$e";
console.log(str.replace(/[!@#$]/g, '')); // output: abcde
```

Podobnie, możesz usunąć wszystkie spacje w ciągu znaków używając wyrażenia regularnego `/ /g` bez wyraźnego określenia znaku, ponieważ spacja jest już znakiem specjalnym.

## Głębsze zagadnienia

Podczas używania wyrażeń regularnych, ważne jest, aby pamiętać o użyciu odpowiednich meta-znaków w zależności od wzoru, którego chcesz uzyskać. Na przykład, jeśli chcesz usunąć wszystkie litery z ciągu znaków, użyj wyrażenia regularnego `/[a-z]/gi`, gdzie `i` oznacza, że jest to niewrażliwe na wielkość liter. 

Możesz również użyć złożonych wyrażeń regularnych, aby usunąć znaki pasujące do różnych wzorców naraz. Na przykład, jeśli chcesz usunąć zarówno litery, jak i cyfry z ciągu znaków, użyj wyrażenia regularnego `/[a-zA-Z0-9]/g`.

Ważne jest również pamiętać o globalnym `g` flag'u, który pozwala na usunięcie wszystkich znaków pasujących do wzoru, a nie tylko pierwszego wystąpienia.

## Zobacz również

Jeśli jesteś zainteresowany bardziej zaawansowanymi technikami stosowanymi w wyrażeniach regularnych, zapoznaj się z poniższymi linkami:

- https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions
- https://www.w3schools.com/jsref/jsref_obj_regexp.asp
- https://www.regular-expressions.info/quickstart.html