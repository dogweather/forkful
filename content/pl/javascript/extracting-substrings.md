---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

---

# Wydobycie Podłańcuchów w JavaScript: Przewodnik Krok po Kroku

## Co i Dlaczego?
Wyodrębnianie podłańcuchów oznacza wycinanie mniejszych części z dłuższego łańcucha znaków. Programiści robią to, aby manipulować danymi, zrozumieć treść łańcucha lub przekształcić go w inny format.

## Jak to Zrobić:

Oto jak wyodrębnić podłańcuchy w JavaScript korzystając z różnych metod.

```Javascript
// Metoda slice(start, end)
let string = 'Programowanie w JavaScript';
let substring = string.slice(0, 14);
console.log(substring); // wyświetli "Programowanie"

// Metoda substring(start, end)
let anotherSubstring = string.substring(15,25);
console.log(anotherSubstring); // wyświetli "JavaScript"

// Metoda substr(start, length)
let yetAnotherSubstring = string.substr(0, 14);
console.log(yetAnotherSubstring); // wyświetli "Programowanie"
```
   
## Wejdźmy w Głębię

Historicznie rzecz ujmując, koncepcja wyodrębniania podłańcuchów jest tak stara jak programowanie komputerowe. Została wprowadzona w celu lepszego zarządzania i manipulowania danymi tekstowymi.
Alternatywą dla wyodrębniania podłańcuchów są reguły wyrażeń regularnych, które oferują bardziej zaawansowane sposoby manipulowania łańcuchami. Niemniej jednak, wyodrębnianie podłańcuchów jest prostsze do zrozumienia i implementacji.
W JavaScript, funkcje jak slice(), substring() oraz substr() są wbudowane w obiekt String i szybko przetwarzają łańcuchy.

## Zobacz Również

Jak już zrozumiesz podstawy wyodrębniania podłańcuchów, możesz zgłębić temat korzystając z poniższych linków:
- [MDN Web Docs - String.prototype.slice()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/slice)
- [MDN Web Docs - String.prototype.substring()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/substring)
- [MDN Web Docs - String.prototype.substr()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/substr)

Ponadto, aby porównać te metody, oto [artykuł na StackOverflow](https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring) wyjaśniający różnice między slice(), substring() i substr().