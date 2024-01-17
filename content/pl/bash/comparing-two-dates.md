---
title:                "Porównywanie dwóch dat"
html_title:           "Bash: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Czym jest porównywanie dwóch dat i dlaczego programiści to robią?

Porównywanie dwóch dat jest procesem polegającym na porównaniu dwóch dat w celu ustalenia, która jest wcześniejsza, późniejsza lub czy są one sobie równe. Programiści często muszą porównywać daty w swoich programach, aby podejmować odpowiednie decyzje lub wyświetlać informacje w odpowiedniej kolejności.

## Jak to zrobić?

Sprawdzenie, która z dwóch dat jest wcześniejsza, późniejsza lub czy są sobie równe w Bashu jest dość proste. Aby tego dokonać, należy użyć porównania operatorem `-gt`, `-lt` lub `=`. Poniżej przedstawiono przykładowy kod, który porównuje dwie daty i wyświetla odpowiednie komunikaty w zależności od wyniku.

```Bash
# Przykładowe daty
data1="2020-01-01"
data2="2019-12-31"

# Porównanie dat
if [[ "$data1" -gt "$data2" ]]; then
  echo "$data1 jest wcześniejsza niż $data2"
elif [[ "$data1" -lt "$data2" ]]; then
  echo "$data1 jest późniejsza niż $data2"
else
  echo "Daty są sobie równe"
fi
```

**Wynik:**

```Bash
2020-01-01 jest wcześniejsza niż 2019-12-31
```

## Głębsze wgląd

Porównywanie dat jest szeroko wykorzystywaną funkcją w Bashu i innych językach programowania. Określenie, która z dwóch dat jest wcześniejsza lub późniejsza, jest często niezbędne do wykonywania różnych operacji, na przykład sortowania lub filtrowania danych. Alternatywnym sposobem na porównywanie dat w Bashu jest użycie polecenia `bc`, które pozwala na wykonywanie obliczeń matematycznych z wykorzystaniem różnych operatorów, w tym porównania `<` i `>`.

W Bashu daty mogą być przechowywane w różnych formatach, na przykład `YYYY-MM-DD` lub `DD-MM-YYYY`. Dlatego ważne jest, aby upewnić się, że daty są w tym samym formacie przed ich porównywaniem.

## Zobacz także

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/)
- [Porównywanie dat z użyciem polecenia 'bc'](https://www.shell-tips.com/bash/math-arithmetic-calculation/#compare)