---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Fish Shell: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem, gdy pracujemy z dużymi plikami tekstowymi, chcemy szybko i sprawnie dokonywać zmian w tekście. W Fish Shell możemy wykorzystać wbudowane funkcje do wyszukiwania i zamiany tekstu, co pozwoli nam zaoszczędzić czas i uniknąć monotonnej pracy ręcznej.

## Jak to zrobić

Fish Shell posiada wbudowaną funkcję `string replace`, która umożliwia nam zmianę wybranego tekstu w całym pliku lub tylko w wybranych liniach. Aby jej użyć, wystarczy podać trzy argumenty w poniższej składni:

```
string replace <tekst_do_zamiany> <nowy_tekst> <plik_tekstowy>
```

Na przykład, jeśli chcemy zmienić wszystkie wystąpienia słowa "kot" na "pies" w pliku `tekst.txt`, możemy wpisać:

```
string replace kot pies tekst.txt
```

Możemy również dokonać zmiany tylko w wybranych liniach, dodając opcjonalny czwarty argument `linie:begin,end`. Przykładowo, jeśli chcemy zmienić tylko słowo "kot" na liniach od 5 do 10, możemy użyć poniższej składni:

```
string replace kot pies tekst.txt linie:5,10
```

## Głębsze wodospady

Funkcja `string replace` w Fish Shell oferuje także możliwość użycia wyrażeń regularnych, co pozwala na jeszcze większą precyzję w zmianie tekstu. Możemy na przykład użyć symbolu `.*` do zastąpienia wszystkich znaków pomiędzy dwoma wybranymi słowami.

Dodatkowo, możemy wykorzystać także funkcję `string match` do wyszukiwania tekstu, a następnie przekazać wynik do funkcji `string replace`, co pozwoli nam dokonać zmiany tylko w wybranych miejscach.

Ogółem, wykorzystanie funkcji `string replace` w Fish Shell pozwala na szybką i elastyczną zamianę tekstu w plikach tekstowych.

## Zobacz także

- Dokumentacja Fish Shell: <https://fishshell.com/docs/current/cmds/string.html#replace-string>
- Przykłady użycia wyrażeń regularnych: <https://www.regular-expressions.info/examples.html>
- Tutorial dotyczący wykorzystania funkcji `string match` i `string replace`: <https://dev.to/lbte/chaining-commands-in-fish-44m0>