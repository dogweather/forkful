---
title:    "TypeScript: Zmiana wielkości liter w ciągu znaków"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Programowanie to skuteczne narzędzie do tworzenia nowych aplikacji, jednak w czasach, gdy wydajność jest kluczowa, znajomość języków programowania jest niezbędna. W tym artykule dowiesz się, dlaczego warto wzbogacić swoją wiedzę o TypeScript poprzez naukę, jak skutecznie kapitalizować stringi.

## Jak to zrobić

Aby poprawnie kapitalizować stringi w TypeScript, należy użyć metody toUpperCase(). Sprawdź poniższy kod, aby zobaczyć jak to zrobić:

```TypeScript
let name = "julia";
console.log(name.toUpperCase());
```

Wynikiem powyższego kodu będzie "JULIA" - wszystkie litery zostały zamienione na wielkie. Jednak, gdy użyjemy tej samej metody na stringu "NOC" wynikiem będzie również "NOC". Dzieje się tak, ponieważ wszystkie litery są już wielkimi literami.

```TypeScript
let word = "NOC";
console.log(word.toUpperCase());
```

W przypadku, gdy chcemy kapitalizować tylko pierwszą literę w stringu, musimy użyć metody charAt() i toUpperCase(). Przykład:

```TypeScript
let word = "mieszko";
let firstLetter = word.charAt(0).toUpperCase();
let restOfWord = word.slice(1);
console.log(firstLetter + restOfWord);
```

Wynikiem powyższego kodu będzie "Mieszko" - tylko pierwsza litera została zmieniona na wielką.

## Deep Dive

Powyższe przykłady są proste i pokazują podstawowe sposoby na kapitalizowanie stringów w TypeScript. Jednak, gdy chcemy bardziej precyzyjnie kontrolować nasze stringi, warto poznać bardziej zaawansowane metody, takie jak replace() czy regularne wyrażenie (RegEx). Przeczytaj dokładnie dokumentację TypeScript, aby dowiedzieć się więcej.

## Zobacz też

Sprawdź poniższe linki, aby dowiedzieć się więcej o kapitalizowaniu stringów w TypeScript:

- https://www.typescriptlang.org/docs/handbook/basic-types.html - oficjalna dokumentacja TypeScript, zawierająca informacje o metodach do manipulacji stringami
- https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Strony_globalne/String/toUpperCase - informacje o metodzie toUpperCase() w języku JavaScript
- http://efektywnyprogramista.pl/legalne-stringify-czyli-null-vs-undefined-w-praktyce/ - artykuł o kapitalizowaniu stringów wraz z przykładami wykorzystującymi RegEx