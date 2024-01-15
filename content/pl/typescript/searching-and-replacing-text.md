---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "TypeScript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Poszukiwanie i zamiana tekstu jest nieodłączną częścią programowania i może pomóc w szybkim i dokładnym wprowadzaniu zmian w kodzie. W przypadku języka TypeScript, umiejętność wyszukiwania i zamiany tekstu jest szczególnie przydatna ze względu na statyczne typowanie i silne zasady typów.

## Jak to zrobić

W języku TypeScript, istnieją dwa sposoby na przeprowadzenie wyszukiwania i zamiany tekstu: przy użyciu wbudowanej metody String `replace` lub za pomocą wyrażeń regularnych.

### Użycie `replace`:

Podstawowe użycie metody String `replace` wygląda następująco:

```TypeScript
const name = "John";
const updatedName = name.replace("John", "Jane");

console.log(updatedName); // Output: Jane
```

Metoda `replace` przyjmuje dwa argumenty: pierwszy to ciąg znaków lub wyrażenie regularne do wyszukania, a drugi to nowa wartość, która zastąpi znalezione wystąpienie.

### Użycie wyrażeń regularnych:

Wyrażenia regularne są potężnym narzędziem w wyszukiwaniu i zamianie tekstu. Przy użyciu flagi globalnej (`g`), można przeprowadzić zamianę wszystkich wystąpień zamiast tylko pierwszego.

Przykład użycia z wykorzystaniem wyrażeń regularnych:

```TypeScript
const string = "There is a cow in the field";

const updatedString = string.replace(/cow/g, "horse");

console.log(updatedString); // Output: There is a horse in the field
```

W tym przykładzie, wyrażenie regularne `/cow/g` będzie wyszukiwać wszystkie wystąpienia słowa "cow" i zamieni je na "horse".

## Głębsza analiza

Podczas dokładniejszego badania wyszukiwania i zamiany tekstu w języku TypeScript, można zauważyć, że metoda `replace` zwraca nowy ciąg znaków, a oryginalny pozostaje bez zmian. Może to być przydatne w przypadku, gdy trzeba zachować oryginalną wartość, ale także wprowadzić zmiany w nowym ciągu.

Ponadto, w przypadku użycia wyrażeń regularnych, można również użyć grup w celu określenia części tekstu, który ma zostać zachowany w nowym ciągu. Na przykład:

```TypeScript
// Zamiana daty z formatu MM/DD/YYYY na DD/MM/YYYY
const dateString = "12/25/2021";

const updatedDateString = dateString.replace(/(\d{2})\/(\d{2})\/(\d{4})/g, "$2/$1/$3");

console.log(updatedDateString); // Output: 25/12/2021
```

W tym przykładzie, grupy `(\d{2})`, `(\d{2})` oraz `(\d{4})` są użyte do wyodrębnienia odpowiednio miesiąca, dnia i roku, a następnie wykorzystywane są w kolejności `$2/$1/$3` w zamianie ciągu.

## Zobacz także

- [Dokumentacja TypeScript: Metoda String `replace`](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#string-replace)
- [Dokumentacja JavaScript: Wyrażenia regularne](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Kurs języka TypeScript na Codecademy](https://www.codecademy.com/learn/learn-typescript)