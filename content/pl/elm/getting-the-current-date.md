---
title:                "Otrzymywanie aktualnej daty"
html_title:           "Elm: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

##

 Cześć programiści!

## O co chodzi & Po co ?

Pobranie aktualnej daty jest krokiem niezbędnym w wielu programach, m.in. w aplikacjach finansowych czy tworzeniu kalendarzy. W Elm jest to proste do zrobienia i często wykorzystywane przez programistów.

## Jak to zrobić:

 ``` Elm
import Time

-- Pobranie aktualnej daty
date = Time.now
-- Wynik: 1628310760703

-- Pobranie aktualnej daty w formacie ISO
isoDate = Time.toIsoString date
-- Wynik: "2021-08-06T12:06:00.703Z"
```

## Głębszy zanurzenie:

Funkcja ```now``` jest częścią modułu ```Time```, który został wprowadzony w wersji 0.19 Elm. Wcześniej, aby uzyskać aktualną datę, trzeba było korzystać z zewnętrznych bibliotek lub interfejsów Javascript.

Alternatywnym sposobem na pobranie bieżącej daty jest użycie funkcji ```inSeconds```, która zwróci liczbę sekund od 1 stycznia 1970 r. Dzięki temu można łatwo obliczyć dowolne przesunięcia czasowe.

Funkcja ```toIsoString``` konwertuje datę na standardowy format ISO. Można również użyć funkcji ```toPosix``` w celu uzyskania daty w formacie numerycznym Unix.

## Zobacz także:

Dokumentacja modułu Time w Elm: https://package.elm-lang.org/packages/elm/time/latest/

Oficjalny poradnik Elm: https://guide.elm-lang.org/