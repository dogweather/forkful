---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Rzutowanie daty na ciąg znaków to konwersja obiektu daty na ciąg tekstowy, taki jak "12-12-2022". Programiści robią to, by łatwiej przekazywać i przechowywać dane daty.

## Jak to zrobić:

W JavaScript, aby przekształcić datę w string, używamy metody `toLocaleDateString()`. Oto przykład:

```Javascript 
let data = new Date();
let dataJakoString = data.toLocaleDateString();
console.log(dataJakoString); // 'dd.mm.yyyy'
```

Ten kod zwraca bieżącą datę jako ciąg znaków w formacie "dd.mm.yyyy".

## Głębsza analiza

#### Kontekst historyczny

JavaScript od samego początku umożliwiał konwersję dat na ciągi znaków. Metody, takie jak `toString()`, `toDateString()`, `toLocaleDateString()`, zawsze były dostępne dla programistów.

#### Alternatywy

Inne metody, takie jak `toISOString()` lub `toJSON()`, również konwertują datę na ciąg znaków, ale w różnych formatach. Przykładowo `toISOString()` zwraca date w formacie ISO 8601, który zawiera datę i czas w formacie UTC.

```Javascript 
let data = new Date();
console.log(data.toISOString()); // 'yyyy-mm-ddT00:00:00.000Z'
```

#### Szczegóły implementacji

`toLocaleDateString()` jest uzupełniana przez argumenty lokalizacji i opcji, które pozwalają na personalizacje formatu daty. Na przykład:

```Javascript
let data = new Date();
let opcje = { year: 'numeric', month: 'long', day: 'numeric' };
console.log(data.toLocaleDateString('pl-PL', opcje)); // "długi dzień miesiąca siódmy roku 2022"
```

## Zobacz także:

1. Dokumentacja MDN na temat [`toLocaleDateString()`](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
2. Wyjaśnienie [Converting a Date object to a string](https://www.w3resource.com/javascript/date-object-toLocaleDateString.php) na W3 Resource
3. Poradnik [Working with JavaScript Date](https://flaviocopes.com/javascript-dates/) na Flavio Copes blogu.