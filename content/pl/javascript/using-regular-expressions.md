---
title:    "Javascript: Używanie wyrażeń regularnych"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions są narzędziem, które bardzo ułatwia pracę z tekstem w Javascript. Pozwalają na wyszukiwanie i manipulowanie danymi w szybki i precyzyjny sposób. Dla programistów jest to niezwykle przydatne narzędzie, ponieważ umożliwia wykonywanie złożonych operacji tekstowych w prosty sposób.

## Jak to zrobić?

### Wyszukiwanie i zastępowanie

Aby wykorzystać regular expressions w swoim kodzie JavaScript, musisz najpierw użyć obiektu `RegExp`. Następnie możesz użyć metody `test()` lub `match()` w celu wyszukania lub dopasowania określonego tekstu.

```javascript
let text = "To jest przykładowy tekst do wyszukania.";
let regex = new RegExp("przykładowy");
console.log(regex.test(text)); // zwróci true
console.log(text.match(regex)); // zwróci tablicę z dopasowanym tekstem
```

### Znak specjalny "."

W regular expressions, znak `.` oznacza dowolny pojedynczy znak. Możesz również użyć go w połączeniu z kwantyfikatorem `*`, aby dopasować dowolną liczbę znaków.

```javascript
let text = "123abc456";
let regex = /.../g;
console.log(text.match(regex)); // zwróci ["123", "abc", "456"]
```

### Kwantyfikatory

Kwantyfikatory pozwalają na dopasowanie określonej liczby znaków. Na przykład, jeśli chcesz dopasować trzy cyfry, możesz użyć kwantyfikatora `{3}` po znaku `\d`, który oznacza cyfrę.

```javascript
let text = "123abc456";
let regex = /\d{3}/g;
console.log(text.match(regex)); // zwróci ["123", "456"]
```

### Grupy i wyrażenia alternatywne

Możesz również używać nawiasów, aby utworzyć grupy w regular expressions. Na przykład, jeśli chcesz dopasować słowo "hello" lub "hi", możesz użyć nawiasów i wyrażenia alternatywnego `|`.

```javascript
let text = "Hello world, hi everyone";
let regex = /(hello|hi)/g;
console.log(text.match(regex)); // zwróci ["Hello", "hi"]
```

## W głębi

Regular expressions to narzędzie, które warto poznać, ponieważ może znacznie ułatwić prace z tekstem w JavaScript. Istnieje wiele sposobów, w jaki można wykorzystać regular expressions, na przykład do walidacji formularzy, filtrowania danych czy manipulacji zapytaniami API.

## Zobacz również

- [Dokumentacja Mozilla o RegExp](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/RegExp)
- [Wprowadzenie do Regular expressions w JavaScript](https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions-in-javascript)