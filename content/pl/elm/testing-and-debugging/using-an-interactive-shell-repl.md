---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases: - /pl/elm/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:13:43.985011-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pętla Czytaj-Ewaluuj-Wypisz (REPL) to proste, interaktywne środowisko programistyczne, które pobiera pojedyncze dane wejściowe od użytkownika, ocenia je i zwraca wynik użytkownikowi. Programiści Elm wykorzystują REPL do szybkich eksperymentów, debugowania czy nauki języka.

## Jak?
Elm nie posiada zintegrowanego REPL. Jednakże, możesz użyć `elm repl` z linii poleceń, aby uruchomić sesję Elm po zainstalowaniu Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

W tej sesji, po zaimportowaniu funkcji Listy, podwoiliśmy liczby na liście i otrzymaliśmy wynik natychmiast.

## Dogłębna analiza
REPL Elm może wydawać się ograniczony w porównaniu z REPL niektórych innych języków takich jak Python czy JavaScript, ponieważ Elm jest językiem kompilowanym skupionym na tworzeniu aplikacji internetowych. Historycznie, Elm skupiał się na pełnych aplikacjach bardziej niż na skryptach czy interakcjach w shellu.

Alternatywy dla REPL Elm obejmują `elm-live` i edytory online takie jak Ellie, gdzie możesz zobaczyć zmiany w kodzie odzwierciedlone w czasie rzeczywistym w przeglądarce.

Jeśli chodzi o implementację, Elm REPL kompiluje fragmenty kodu Elm do JavaScriptu w tle, pozwalając na interaktywne uruchamianie Elm. Jest to różne od REPL języków interpretowanych, które nie wymagają tego kroku kompilacji. REPL Elm jest również uproszczony, aby jądro języka było lekkie i skoncentrowane.

## Zobacz również
- Oficjalny przewodnik Elm na temat interaktywności: https://guide.elm-lang.org/interop/
- Ellie, online plac zabaw Elm: https://ellie-app.com/new
- `elm-live`, elastyczny serwer deweloperski dla Elm: https://www.elm-live.com/
