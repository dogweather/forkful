---
title:                "TypeScript: Wyszukiwanie i zamiana tekstu"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek znalazłeś się w sytuacji, gdzie musiałeś zmienić kilka słów lub fraz w swoim kodzie? Może to była zmiana nazwy zmiennej lub poprawienie błędu w tekście. Bez względu na powód, wyszukiwanie i zastępowanie tekstu jest niezwykle pomocne w programowaniu. Pozwala na szybkie i efektywne wprowadzanie zmian w kodzie oraz oszczędza czas i wysiłek programisty.

## Jak to zrobić

Aby przeszukiwać i zamieniać tekst w TypeScript, możemy użyć funkcji `replace()`. Ta metoda przyjmuje dwa argumenty: pierwszy to szukany tekst, a drugi to tekst, który ma zostać wstawiony w miejsce znalezionego tekstu. Najbardziej przydatną opcją w tej funkcji jest dodanie flagi globalnej, dzięki której przeszukiwanie będzie obejmować cały tekst, a nie tylko pierwsze znalezione wystąpienie. Poniżej znajduje się przykładowy kod wykorzystujący funkcję `replace()`.

```TypeScript
const text = 'Cześć, nazywam się Jan, mam 25 lat.';
const newText = text.replace(/Jan/g, 'Anna');
// Nowy tekst: Cześć, nazywam się Anna, mam 25 lat.
```

Jak widać powyżej, szukamy słowa "Jan" za pomocą wyrażenia regularnego `/Jan/` i zastępujemy je słowem "Anna". Dzięki dodaniu flagi globalnej `g`, funkcja `replace()` przeszuka cały tekst i zamieni wszystkie wystąpienia.

## Deep Dive

Istnieje wiele różnych zastosowań funkcji `replace()` w TypeScript. Możemy na przykład użyć jej do zmiany wielkości liter w wyrazach, wykasowania znaków specjalnych lub dodania prefiksu do wyrazów. Poniżej znajdują się przykładowe kody prezentujące te możliwości.

```TypeScript
// Zmiana wielkości liter
const text = 'Programowanie jest super!';
const newText = text.replace(/super/, 'SUPER');
// Nowy tekst: Programowanie jest SUPER!

// Usunięcie znaków specjalnych
const text = 'F#^a@n%t^a$s#t&y!';
const newText = text.replace(/[\^@%#&]/g, '');
// Nowy tekst: Fantasy!

// Dodanie prefiksu
const text = 'Pies, kot, chomik';
const newText = text.replace(/(\w+)/g, 'moj$1');
// Nowy tekst: mojPies, mojKot, mojChomik
```

Dodatkowo, możemy także użyć funkcji `replace()` w połączeniu z funkcją `match()`, która zwraca tablicę znalezionych wyrażeń, aby przeszukiwać i zastępować bardziej złożone wzorce.

## Zobacz też

- [Dokumentacja funkcji `replace()`](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/replace)
- [Wzorce regularne w TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Funkcja `match()` w TypeScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Globalne/String/match)