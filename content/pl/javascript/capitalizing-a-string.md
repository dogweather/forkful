---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja stringów (ciągów znaków) to zmiana pierwszych liter na duże. Czynimy to, by tekst wyglądał estetycznie, był czytelny lub spełniał standardy formatowania.

## Jak to zrobić:
```javascript
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('witaj świecie')); // Wynik: 'Witaj świecie'
```

## Zagłębiając się:
Kapitalizacja stringów jest stara jak świat programowania. W przeszłości każda nowa technologia wprowadzała własne metody. Dziś, JavaScript oferuje metody jak `toUpperCase()` i operacje na stringach takie jak `charAt()`, które ułatwiają zadanie. Alternatywą jest wyrażenia regularne czy biblioteki pomocnicze jak Lodash z funkcją `_.capitalize`. Co ciekawe, nie każda językowa implementacja kapitalizacji radzi sobie z polskimi znakami – trzeba więc uważać na „ł” czy „ś”.

## Zobacz również:
- Dokumentacja MDN na temat metody `toUpperCase()`: [MDN toUpperCase](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- Artykuł o wyrażeniach regularnych: [Wyrażenia regularne w JS](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)
- Biblioteka Lodash i metoda `capitalize`: [Lodash capitalize](https://lodash.com/docs/#capitalize)
