---
title:                "Porównywanie dwóch dat"
html_title:           "Fish Shell: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

Cześć czytelnikach!

Dzisiaj omówimy temat porównywania dwóch dat w języku programowania "Fish Shell". Możesz zastanawiać się, co to właściwie jest i dlaczego programiści to robią. Nie martw się, opowiem Ci o tym w kilku krótkich zdaniach.

## Co i dlaczego?

Porównywanie dwóch dat jest procesem, w którym program porównuje dwie różne daty i określa, która jest wcześniejsza lub późniejsza. Programiści często to robią w celu ustalenia kolejności wydarzeń lub sprawdzenia, czy data jest ważniejsza lub aktualniejsza.

## Jak to zrobić?

Fish Shell ma wbudowane narzędzie do porównywania dat - funkcję `date`. Aby skorzystać z niej, użyj następującego polecenia w terminalu:

```Fish Shell
date --date "data1" +%s
date --date "data2" +%s
```

Zamiast "data1" i "data2" wpisz odpowiednie daty, które chcesz porównać. Program zwróci liczby, które reprezentują dane daty w formacie Unix. Porównanie liczb da Ci informację, która data jest wcześniejsza lub późniejsza.

## Dlaczego to działa?

Funkcja `date` działa dzięki wykorzystaniu polecenia `date` z systemowego polecenia Linux. Wartość `%s` w funkcji `date` oznacza czas w formacie Unix - ilość sekund od 1 stycznia 1970 roku. Porównanie tych wartości daje nam odpowiedź, która data jest wcześniejsza lub późniejsza.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o funkcji `date` w języku Fish Shell, możesz przeczytać dokumentację [tutaj](https://fishshell.com/docs/current/cmds/date.html).

Teraz już wiesz, jak porównywać daty w języku Fish Shell. To proste narzędzie może być bardzo przydatne w wielu sytuacjach, więc koniecznie spróbuj go wykorzystać w swoim kodzie. Dzięki temu będziesz mógł łatwo ustalić, która data jest wcześniejsza lub późniejsza. Do zobaczenia w kolejnym artykule!