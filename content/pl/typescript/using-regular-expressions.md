---
title:                "TypeScript: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego warto używać wyrażeń regularnych w programowaniu TypeScript?

Wyrażenia regularne są bardzo potężnym narzędziem, które pozwala na szybkie i wydajne przetwarzanie tekstu. W programowaniu TypeScript mogą być wykorzystywane do walidacji danych, wyszukiwania wzorców w tekście oraz wielu innych zastosowań. Dzięki nim nasz kod staje się bardziej czytelny i łatwiejszy w utrzymaniu.

## Jak używać wyrażeń regularnych w programowaniu TypeScript?

```TypeScript
// Przykład używania wyrażenia regularnego w TypeScript.
let string = "Hello World!";
let pattern = /Hello/;

console.log(pattern.test(string));
// Output: true
```

W powyższym przykładzie używamy wyrażenia regularnego `/Hello/` do sprawdzenia czy w tekście "Hello World!" znajduje się fraza "Hello". Metoda `test()` zwraca `true` gdy spełniony jest warunek, a `false` w przeciwnym wypadku.

Wyrażenia regularne mogą również zawierać kilka różnych symboli, które pozwalają na bardziej zaawansowane operacje. Na przykład:
- `.` - pasuje do każdego pojedynczego znaku
- `*` - pasuje do dowolnej liczby wystąpień poprzedniego znaku
- `\s` - pasuje do białego znaku (spacji, tabulacji, nowej linii)
- `[]` - pasuje do jednego znaku z podanego zbioru

Powyższe symbole można łączyć w celu stworzenia bardziej skomplikowanych wyrażeń regularnych. Jest to jednak tylko krótka zapowiedź, aby naprawdę nauczyć się wyrażeń regularnych w TypeScript, trzeba poświęcić więcej czasu i praktyki.

## Głębsze spojrzenie na wyrażenia regularne w programowaniu TypeScript

Wyrażenia regularne w TypeScript są naprawdę złożonym tematem i mogą być wykorzystywane w różnych celach. Mogą być używane do przetwarzania dużych plików tekstowych, weryfikacji złożonych wzorców danych, czy nawet do wykrywania wyrażeń regularnych w innych wyrażeniach regularnych.

Jedną z najważniejszych funkcji wyrażeń regularnych jest możliwość użycia tzw. grupowanie, czyli określenia fragmentów wzorca jako grupy i późniejszego wykorzystania ich w kodzie. Na przykład:

```TypeScript
// Grupowanie numeru telefonu w wyrażeniu regularnym
let pattern = /(\d{3})-(\d{3})-(\d{4})/;
let string = "555-123-4567";
let match = string.match(pattern);

console.log(match[1]);
// Output: 555
console.log(match[2]);
// Output: 123
console.log(match[3]);
// Output: 4567
```

Powyższy przykład pokazuje jak używać grupowania w wyrażeniach regularnych. Dzięki temu możemy wyodrębnić konkretne części tekstu i wykorzystać je w dalszej części kodu.

Ważnym aspektem wyrażeń regularnych jest również ich wydajność. W programowaniu TypeScript można wykorzystać flagi, które zmieniają sposób działania wyrażenia regularnego. Na przykład flaga `g` pozwala na globalne wyszukanie wzorca w całym tekście, a flaga `i` ignoruje różnicę między małymi i wielkimi literami.

## Zobacz także

- [Dokumentacja TypeScript na temat wyrażeń regularnych](https://www.typescriptlang.org/docs/handbook/regexp.html)
- [W3Schools - Wyrażenia regularne](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Regex101 - Narzędzie online do tworzenia i testowania