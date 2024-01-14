---
title:    "TypeScript: Używanie wyrażeń regularnych"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego warto używać wyrażeń regularnych?

Wyrażenia regularne są niezwykle przydatnym narzędziem w programowaniu, pozwalającym na wykonywanie zaawansowanych operacji na tekstach, takich jak wyszukiwanie, wycinanie lub zastępowanie określonych wzorców. Są niezwykle wszechstronne i potrafią przyspieszyć proces tworzenia oprogramowania. Jeśli pracujesz z dużymi ilościami danych tekstowych lub często musisz przetwarzać dane wejściowe, warto poznać wyrażenia regularne i wykorzystać je w swoim kodzie.

## Jak używać wyrażeń regularnych w TypeScript?

Aby zacząć korzystać z wyrażeń regularnych w TypeScript, musisz najpierw utworzyć obiekt klasy `RegExp`, który będzie przechowywał szukany wzorzec. Następnie możesz wykorzystać różne metody tej klasy, takie jak `test()` czy `exec()`, aby przeprowadzić operacje na tekście. Poniżej znajdują się dwa przykładowe kody, które wyjaśniają jak wyszukać i podmienić określone wzorce w tekście:

```
// Przykład 1: Wyszukiwanie słowa "Hello" w tekście
let tekst = "Witaj na blogu o programowaniu w TypeScript!";
let szukanyWzorzec = /Hello/;
let czyZawiera = szukanyWzorzec.test(tekst);
console.log(czyZawiera); // Output: false (ponieważ tekst nie zawiera słowa "Hello")

// Przykład 2: Zamiana daty z anglojęzycznego formatu "YYYY-MM-DD" na format polżczyzny "DD.MM.YYYY"
let data = "2020-11-30";
let szukanyWzorzec = /(\d{4})-(\d{2})-(\d{2})/;
let dataWynikowa = data.replace(szukanyWzorzec, "$3.$2.$1");
console.log(dataWynikowa); // Output: "30.11.2020"
```

## Głębsza analiza wyrażeń regularnych

Wyrażenia regularne w TypeScript są oparte na standardzie ECMAScript, co oznacza że wykorzystują podobną składnię i mechanizmy co w innych językach programowania (takich jak JavaScript czy Python). Jednym z najważniejszych aspektów wyrażeń regularnych jest możliwość kontrolowania ich dokładności poprzez użycie tzw. kwantyfikatorów, takich jak `?`, `+` czy `*`, aby określić ilość wystąpień szukanego wzorca. Ponadto, można także korzystać z tzw. grup, czyli ujmowania fragmentów wzorca w nawiasy i wykorzystania ich później w zastępowaniu. Jest to bardzo przydatne przy wykonywaniu skomplikowanych operacji na tekście.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych w TypeScript, polecam zapoznać się z oficjalną dokumentacją języka oraz z poradnikami dostępnymi online:

- Oficjalna dokumentacja TypeScript: https://www.typescriptlang.org/docs/
- Poradnik wyrażeń regularnych w ECMAScript: https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions
- Przykładowe kody z wykorzystaniem wyrażeń regularnych: https://regex101.com/