---
date: 2024-01-20 17:31:03.782641-07:00
description: "Obliczenie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to\
  \ po prostu wyznaczenie dat r\xF3\u017Cni\u0105cych si\u0119 o ustalon\u0105 liczb\u0119\
  \ dni od dzi\u015B. Programi\u015Bci robi\u0105 to, by\u2026"
lastmod: '2024-03-13T22:44:35.855164-06:00'
model: gpt-4-1106-preview
summary: "Obliczenie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to po\
  \ prostu wyznaczenie dat r\xF3\u017Cni\u0105cych si\u0119 o ustalon\u0105 liczb\u0119\
  \ dni od dzi\u015B."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Jak to zrobić:
W fish można użyć polecenia `date` z odpowiednimi opcjami. Poniżej znajduje się kilka przykładów:

```Fish Shell
# Data za 5 dni
set future_date (date -d "+5 days" +"%Y-%m-%d")
echo $future_date

# Data sprzed tygodnia
set past_date (date -d "-1 week" +"%Y-%m-%d")
echo $past_date
```

Przykładowy wynik:
```
2023-04-12 # Dla daty za 5 dni, zakładając że dziś jest '2023-04-07'
2023-03-31 # Dla daty tydzień temu
```

## Szczegółowo:
Historia: `date` to klasyczne narzędzie Unixowe, służące do wyświetlania i ustawiania daty systemowej. W systemach podobnych do Unix, w tym Linux, macOS, posiadło ono rozszerzenia pozwalające na manipulowanie datami.

Alternatywy: W innych powłokach skryptowych, jak Bash czy Zsh, obliczenia na datach wykonywane są podobnie. Warto znać wbudowane funkcje i narzędzia, takie jak `dateutils`, które oferują szerszą funkcjonalność.

Szczegóły implementacyjne: Poza poleceniem `date`, Fish nie zawiera wbudowanych funkcji do pracy z datami, ale można integrować zewnętrzne narzędzia lub wykorzystać wtyczki i funkcje dostępne przez Fisherman czy Oh My Fish.

## Zobacz także:
- Oficjalna dokumentacja Fish Shell: [fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Wtyczki Fish: [github.com/jorgebucaran/awesome-fish](https://github.com/jorgebucaran/awesome-fish)
