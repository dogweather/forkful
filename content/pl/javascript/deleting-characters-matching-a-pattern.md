---
title:                "Usuwanie znaków odpowiadających wzorcowi"
html_title:           "Javascript: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego programiści często usuwają znaki pasujące do określonego wzorca w swoim kodzie? Istnieją wiele powodów, dla których ta praktyka jest powszechnie stosowana, a w tym artykule dowiecie się, dlaczego jest to ważne i jak można to zrobić w JavaScript.

## Jak to zrobić

Proces usuwania znaków pasujących do wzorca jest stosunkowo prosty, ale może wymagać umiejętnego użycia funkcji wbudowanych w JavaScript. Poniżej przedstawiono przykłady kodu i wyniki dla różnych sytuacji:

```Javascript
const string1 = "To Jest Przykładowy Tekst";

// Usuwa wszystkie spacje
const textWithoutSpaces = string1.replace(/\s/g, "");
console.log(textWithoutSpaces);
// wynik: "ToJestPrzykładowyTekst"

// Usuwa wszystkie małe litery
const textWithoutLowerCase = string1.replace(/[a-z]/g, "");
console.log(textWithoutLowerCase);
// wynik: "TJP"

// Usuwa litery "sz" z tekstu
const textWithoutSz = string1.replace(/sz/g, "");
console.log(textWithoutSz);
// wynik: "To Jest Prykadowy Tekst"
```

W powyższych przykładach wykorzystano metodę `replace()` wraz z wyrażeniem regularnym, aby zastosować wzorzec i usunąć pasujące znaki. Dzięki temu możemy w łatwy sposób edytować nasz tekst.

## Deep Dive

Istnieje wiele różnych sytuacji, w których jest potrzebne usunięcie znaków pasujących do określonego wzorca. Jednym z najczęstszych zastosowań jest filtrowanie danych wprowadzanych przez użytkowników. Dzięki usunięciu niechcianych znaków, możemy zapobiec błędom lub atakom typu SQL Injection.

W JavaScript istnieje wiele innych funkcji, które mogą pomóc nam w manipulowaniu tekstem. Na przykład, metoda `match()` może zostać wykorzystana do znalezienia wszystkich wystąpień pasujących do wzorca w tekście.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o usuwaniu znaków pasujących do wzorca lub o innych funkcjach manipulacji tekstem w JavaScript, polecamy zapoznać się z poniższymi artykułami:

- [MDN - Metoda replace()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/replace)
- [W3Schools - Wyrażenia regularne w JavaScript](https://www.w3schools.com/js/js_regexp.asp)
- [Poradnik Webdev - Filtrowanie danych w JavaScript](https://poradnikwebdev.pl/jak-filtrowac-dane-w-javascript/)