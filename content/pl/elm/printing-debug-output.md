---
title:    "Elm: Drukowanie wyjścia debugowania"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą pracującym w Elm, pewnie zastanawiasz się, po co drukować wyjścia debugowania. Jednym z najważniejszych powodów jest to, że pomaga to zrozumieć, jak działają Twoje funkcje i moduły. Może się to także przydać w przypadku napotkania błędów lub problemów w kodzie.

## Jak to zrobić

W Elm istnieje wbudowana funkcja `Debug.log`, która służy do wyświetlania wyjścia debugowania. Najprostszy sposób jej użycia wygląda następująco:

```
Elm.debug "Twoje wyjście debugowania"
```

Możesz również wyświetlić wartości zmiennych lub wyrażeń, wpisując je po przecinku w nawiasach:

```
Elm.debug (x, y + z)
```

Pamiętaj, że `Debug.log` jest funkcją czysto debugującą i nie powinna być używana w kodzie produkcyjnym.

## Głębsze zagłębienie

Poza prostym wyświetlaniem tekstu, `Debug.log` ma wiele zastosowań, dlatego warto poznać dokładniej jego możliwości.

Możesz np. wyświetlić informacje o typach danych, co jest szczególnie przydatne podczas pracy z Elm Debuggerem. Można to zrobić w ten sposób:

```
Elm.debug (typeof model, typeof msg)
```

Warto również wiedzieć, że `Debug.log` może być zagnieżdżona w innych funkcjach lub wyrażeniach warunkowych. Po prostu umieść ją wewnątrz nawiasów, tak jakbyś używał jej poza nimi.

## Zobacz także

- [Dokumentacja Elm do funkcji `Debug.log`](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)
- [Elm Debugger - narzędzie do debugowania w przeglądarce](https://debug.elm-lang.org)