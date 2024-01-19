---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konkatenacja łańcuchów to proces łączenia dwóch lub więcej łańcuchów znaków w jeden. Programiści używają jej do łączenia różnych informacji, tworzenia wiadomości i udostępniania efektywnych wyjść do użytkownika.

## Jak to zrobić:

W JavaScript można łatwo połączyć łańcuchy znaków za pomocą operatora `+`:

```Javascript
let powitanie = "Cześć, ";
let imie = "Jan";
let powitanieCale = powitanie + imie;
console.log(powitanieCale); // Wypisuje: "Cześć, Jan"
```

Można również użyć metody `.concat()`:

```Javascript
let powitanie = "Cześć, ";
let imie = "Jan";
let powitanieCale = powitanie.concat(imie);
console.log(powitanieCale); // Wypisuje: "Cześć, Jan"
```

## Głębsze zagadnienia:

Historia: Pierwotnie, konkatenacja była używana w wczesnych językach programowania takich jak COBOL czy Fortran. Zasada działania jest identyczna jak w JavaScript.

Alternatywy: Jest kilka alternatywnych sposobów łączenia łańcuchów w JavaScript. Można użyć metody `.join()`, która łączy elementy tablicy w jeden łańcuch, lub „Template Literals”:

```Javascript
let powitanie = "Cześć, ";
let imie = "Jan";
let powitanieCale = `${powitanie}${imie}`;
console.log(powitanieCale); // Wypisuje: "Cześć, Jan"
```

Szczegóły implementacji: Operator `+` lub metoda `.concat()` łączy łańcuchy w kolejności, w jakiej są podawane. Wszystko, co nie jest łańcuchem, jest konwertowane na łańcuch.

## Zobacz również:

1. [MDN Web Docs: String.prototype.concat()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/concat)
2. [MDN Web Docs: Template literals (szablony łańcuchów)](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/template_strings)
3. [JavaScript Info: Konkatenacja łańcuchów](https://pl.javascript.info/string#konkatenacja-czyli-laczenie-lancuchow) 
4. [W3Schools: JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)