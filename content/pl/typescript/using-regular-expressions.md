---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "TypeScript: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Używanie wyrażeń regularnych jest niezbędne dla wszystkich programistów, którzy potrzebują wykonywać operacje takie jak wyszukiwanie, wyodrębnianie i zastępowanie tekstu w dużych ilościach danych. Wyrażenia regularne są szybkie, skuteczne i potrafią oszczędzić dużo czasu, dlatego warto nauczyć się ich używać.

## Jak to zrobić

```TypeScript
// Przykład 1
const regexEmail = /\w+@\w+\.com/;
const email = "janek@firma.com";

console.log(email.match(regexEmail)); 
// Output: ["janek@firma.com"]
```

```TypeScript
// Przykład 2
const regexNumbers = /\d+/g;
const text = "Ala ma 3 koty i 2 psy.";

console.log(text.match(regexNumbers));
// Output: ["3", "2"]
```

Wyrażenie regularne tworzy się poprzez użycie specjalnych znaków i symboli, które określają wzorzec poszukiwanego tekstu. Główne metaznaki to: `+` - jeden lub więcej wystąpień, `*` - zero lub więcej wystąpień, `?` - zero lub jedno wystąpienie, `.` - dowolny znak, `^` - początek linii, `$` - koniec linii. Można również używać wyrażeń przeciwnych, np. `[^a-z]` - znak niebędący małym literą. Podczas używania wyrażeń regularnych, należy uważać na znaki specjalne, np. `\`,które trzeba dwukrotnie escape'ować. 

## Wnikliwe spojrzenie

Wyrażenia regularne są obsługiwane przez klasę `RegExp`, która zawiera metody takie jak `test()` - sprawdzająca czy dany tekst pasuje do wzorca, `exec()` - wyszukująca pierwsze dopasowanie do wzorca, `match()` - wyszukująca wszystkie dopasowania do wzorca w tekście, `replace()` - zamieniająca w tekście wszystkie dopasowania na nowy ciąg znaków. Istnieją również funkcje globalne, takie jak `search()` - szukająca pierwsze wystąpienie wzorca w tekście, `split()` - dzieląca tekst na podstawie wzorca. Wyrażenia regularne mogą również zawierać grupy, które można później odszukać po wykonaniu dopasowania.

## Zobacz również

- [Dokumentacja dla klasa `RegExp`](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/RegExp)
- [Podstawy wyrażeń regularnych w TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)