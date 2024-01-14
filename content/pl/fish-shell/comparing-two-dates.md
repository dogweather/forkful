---
title:                "Fish Shell: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest nieodłączną częścią wielu projektów programistycznych. Często jest to niezbędne do filtrowania danych lub wykonywania operacji na określonych przedziałach czasowych. W tym blogu dowiesz się, jak w łatwy sposób porównywać dwie daty za pomocą powłoki Fish Shell.

## Jak

Porównanie dwóch dat za pomocą powłoki Fish Shell jest bardzo proste. Wystarczy użyć wbudowanej komendy `date`, która pozwoli nam na łatwe porównywanie dat na podstawie różnych kryteriów.

```
Fish Shell (porównanie wersji Fish 3.0.2)
=================
$ date -u +%s -d "01/01/2020"
1577836800

$ date -u +%s -d "01/01/2021"
1609459200
```

W powyższym przykładzie użyliśmy flagi `-u` dla komendy `date`, która pozwala na wyświetlanie daty w uniwersalnym czasie koordynowanym (UTC). Następnie użyliśmy flagi `+%s`, która pozwala na wyświetlenie daty jako liczby sekund od 1 stycznia 1970 roku. W ten sposób porównujemy daty poprzez porównywanie liczb, co jest bardzo proste i efektywne.

Możemy również porównywać daty na podstawie innych kryteriów, na przykład wyświetlając tylko dzień, miesiąc lub rok.

```
Fish Shell (porównanie wersji Fish 3.0.2)
=================
$ date -u +%d -d "01/01/2020"
01

$ date -u +%m -d "01/01/2020"
01

$ date -u +%Y -d "01/01/2020"
2020
```

Możemy także wykorzystać porównanie dat do filtrowania danych. Na przykład, jeśli chcemy wyświetlić tylko te wpisy w pliku dziennika, które zostały utworzone po 1 stycznia 2020 roku, możemy skorzystać z polecenia `grep`.

```
Fish Shell (porównanie wersji Fish 3.0.2)
=================
$ grep "01/01/2020" dziennik.log
Wpis 1 - 01/01/2020
Wpis 2 - 02/05/2020
Wpis 3 - 03/12/2020
```

W ten sposób możemy prosto i szybko filtrować dane oparte na porównywaniu dat.

## Deep Dive

Jeśli chcesz się dowiedzieć więcej o porównywaniu dat, warto zapoznać się z dokumentacją powłoki Fish Shell oraz z opcjami komendy `date`, która oferuje wiele możliwości manipulacji i formatowania daty. Ponadto, w Internecie dostępnych jest wiele przydatnych poradników i przykładów wykorzystania porównywania dat w Fish Shell.

## Zobacz również

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/
- Komenda `date`: https://fishshell.com/docs/current/commands.html#date
- Przykłady wykorzystania porównywania dat w Fish Shell: <link do przykładowych stron>