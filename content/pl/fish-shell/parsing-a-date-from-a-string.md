---
title:                "Parsowanie daty z ciągu znaków"
html_title:           "Fish Shell: Parsowanie daty z ciągu znaków"
simple_title:         "Parsowanie daty z ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Analiza daty z ciągu znaków to proces wyodrębnienia informacji o dacie z tekstu, w którym może ona być umieszczona w różnych formatach. Programiści stosują to, aby dokładniej manipulować danymi i wykonując bardziej precyzyjne operacje na dacie, takie jak sortowanie lub wyświetlanie rekordów z określonego zakresu czasu.

## Jak to zrobić:
Kodując w języku Fish Shell, istnieje kilka przydatnych poleceń do analizy daty z ciągu znaków. Poniżej znajduje się przykład kodu i jego wynik dla daty w formacie rrrr-mm-dd zapisanej w zmiennej ```$date```.

```
# Przykład kodu
set date "2020-10-23"
date -f "%Y-%m-%d" $date

# Wynik
23 października 2020
```

## Głębsze zagłębienie:
Parsowanie daty z ciągu znaków ma długą historię, zaczynającą się w latach 60-tych. Istnieje wiele różnych metod i narzędzi do analizy daty, a wybór zależy od preferencji i potrzeb programisty. Alternatywami dla Fish Shell są między innymi programy takie jak Perl czy Python, które mają własne metody analizy daty. W Fish Shell można również skorzystać z poleceń ```strptime``` i ```strftime``` dla bardziej zaawansowanych zastosowań.

## Zobacz też:
- Oficjalna dokumentacja Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Informacje o polecanuch `date` w Fish Shell: [https://fishshell.com/docs/current/commands.html#date](https://fishshell.com/docs/current/commands.html#date)
- Sposób użycia poleceń `strptime` i `strftime` w Fish Shell: [https://fishshell.com/docs/current/cmds/strftime.html](https://fishshell.com/docs/current/cmds/strftime.html)