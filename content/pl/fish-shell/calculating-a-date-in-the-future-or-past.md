---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Fish Shell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Niektórzy z nas lubią planować rzeczy z wyprzedzeniem, na przykład spotkania z przyjaciółmi czy ważne wydarzenia. Wtedy przydatne może okazać się obliczenie daty w przyszłości lub w przeszłości. W tym artykule dowiesz się, jak wykorzystać Fish Shell do tego celu.

## Jak to zrobić

Obliczanie daty w przyszłości lub w przeszłości jest możliwe dzięki użyciu polecenia `date` w Fish Shell. Przykładowy kod wyglądałby następująco:

```Fish Shell
date -v +1d
```

W powyższym przykładzie, dodaje się 1 dzień do aktualnej daty, co daje nam datę jutrzejszą. Podobnie, używając `-v -1d`, uzyskamy datę wczorajszą.

Jeśli chcemy obliczyć datę w przyszłości lub w przeszłości w oparciu o inną datę niż dzisiejsza, możemy użyć flagi `-j` oraz określić konkretną datę. Przykładowo:

```Fish Shell
date -j -v +2m +5d 06101300
```

W powyższym przykładzie, dodajemy 2 miesiące i 5 dni do daty 06/10/1300, uzyskując 16/12/1300.

## Dogłębna analiza

Fish Shell wykorzystuje komendę `date` dostępną również w innych powłokach Uniksa, takich jak Bash czy Zsh. Dzięki wykorzystaniu flagi `-v`, możemy dodawać lub odejmować określoną wartość od aktualnej daty, określając jednostki czasu za pomocą liter.

Możemy również użyć flagi `-j`, aby podać konkretną datę zamiast aktualnej, na podstawie której będą dokonywane obliczenia. W ten sposób, możemy planować wydarzenia w odległej przyszłości lub cofać się w czasie.

## Zobacz również

- Oficjalna dokumentacja Fish Shell dla polecenia `date`: https://fishshell.com/docs/current/cmds/date.html
- Przewodnik po komendzie `date` w Linuxie: https://www.howtogeek.com/217562/how-to-use-the-date-command-on-linux/
- Artykuł na temat obliczania dat w przeszłości lub przyszłości w Fish Shell: https://www.maketecheasier.com/calculate-date-future-past-fish-shell/