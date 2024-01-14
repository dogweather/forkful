---
title:                "TypeScript: Szukanie i zastępowanie tekstu"
simple_title:         "Szukanie i zastępowanie tekstu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś miałby chcieć przeszukiwać i zmieniać tekst? W codziennym życiu, często musimy modyfikować dokumenty lub pliki tekstowe, aby spełniały nasze potrzeby. Przeszukiwanie i zastępowanie tekstu jest jednym z najbardziej przydatnych narzędzi programistycznych, które może przyspieszyć naszą pracę i usunąć frustrację z manualnego edytowania tekstu.

## Jak to zrobić

Aby przeszukiwać i zastępować tekst w TypeScript, musimy użyć podstawowej metody `replace()` na dowolnym ciągu znaków. Poniższy przykład pokazuje, jak przeszukać i zastąpić wszystkie wystąpienia słowa "hello" w ciągu znaków "hello world" na "hi".

```TypeScript
let text: string = "hello world";
console.log(text.replace("hello", "hi"));
```

Output: `hi world`

Możemy także wykorzystać wyrażenie regularne, aby wykonać bardziej zaawansowane wyszukiwanie i zastępowanie. W tym przykładzie, zamienimy wszystkie litery "a" na "b" w ciągu "Abracadabra".

```TypeScript
let text: string = "Abracadabra";
console.log(text.replace(/a/g, "b"));
```

Output: `Bbrbcbdbbrb`

## Głębszy zanurzenie

Podczas korzystania z metody `replace()`, chcemy być świadomi kilku ważnych rzeczy. Pierwszą z nich jest to, że metoda ta zwraca nowy ciąg znaków i nie modyfikuje oryginalnego. Warto również pamiętać, że metoda ta jest case-sensitive, co oznacza, że jest wrażliwa na wielkość liter.

Inną przydatną rzeczą jest to, że w przypadku użycia wyrażenia regularnego, możemy również przekazać funkcję jako drugi argument metody `replace()`. Funkcja ta będzie wywoływana dla każdego dopasowania wyrażenia regularnego i jej zwrócony wynik zostanie użyty do zastąpienia dopasowanych znaków. Pozwala to na bardziej elastyczne i zaawansowane przetwarzanie tekstu.

## Zobacz także

- [Dokumentacja dla metody `replace()` w TypeScript](https://www.typescriptlang.org/docs/handbook/string-based-types.html#the-string-replace-method)
- [Wprowadzenie do wyrażeń regularnych w projektowaniu stron internetowych](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)