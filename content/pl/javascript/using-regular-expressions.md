---
title:                "Javascript: Używanie wyrażeń regularnych"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Dlaczego regularne wyrażenia są ważne w programowaniu?

Regularne wyrażenia są ważnym narzędziem w programowaniu, pozwalającym na skuteczne manipulowanie tekstami i sprawdzanie czy spełniają one określone kryteria. Pozwalają one także na szybkie i prostsze przetwarzanie danych, oszczędzając czas i wysiłek programistów.

## Jak korzystać z regularnych wyrażeń?

Aby używać regularnych wyrażeń w języku Javascript, należy użyć wbudowanego obiektu RegExp. Istnieją trzy sposoby na utworzenie obiektu RegExp: literalnie, z wykorzystaniem konstruktora lub wyrażenia z filtrami.

Przykładowe kodowanie using literal:
```Javascript
var regex = /abc/; 
```

Przykładowe kodowanie using constructor:
```Javascript
var regex = new RegExp("abc"); 
```

Przykładowe coding using expression with flags:
```Javascript
var regex = new RegExp("abc", "gi");
```

## Zagłębienie

Regularne wyrażenia posiadają szerokie zastosowanie w programowaniu. Pozwalają na wykonywanie różnych operacji, takich jak wyszukiwanie, zastępowanie, rozdzielenie i inne. Są również bardzo przydatne w walidacji formularzy i sprawdzaniu poprawności wprowadzonych danych.

Poniżej przedstawione są niektóre przydatne wzory, które można wykorzystać przy korzystaniu z regularnych wyrażeń:

- \d - odpowiada dowolnej cyfrze
- \w - odpowiada dowolnemu znakowi alfanumerycznemu
- \s - odpowiada dowolnemu białemu znakowi
- [abc] - odpowiada każdemu ze znaków a, b lub c
- [^abc] - odpowiada każdemu znakowi, który nie jest a, b lub c
- ^ - odpowiada początkowi tekstu
- $ - odpowiada końcowi tekstu

## Zobacz także

- Dokumentacja języka Javascript: https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions
- Interaktywny tutorial regularnych wyrażeń: https://regexone.com/
- Narzędzie do testowania i weryfikacji regularnych wyrażeń: https://regex101.com/