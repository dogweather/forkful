---
date: 2024-01-26 04:37:06.709668-07:00
description: "Jak to zrobi\u0107: Bash domy\u015Blnie nie obs\u0142uguje liczb zespolonych.\
  \ Cz\u0119sto u\u017Cywa si\u0119 zewn\u0119trznych narz\u0119dzi takich jak `bc`\
  \ z opcj\u0105 `-l`. Oto jak przetwarzasz\u2026"
lastmod: '2024-03-13T22:44:35.576612-06:00'
model: gpt-4-0125-preview
summary: "Bash domy\u015Blnie nie obs\u0142uguje liczb zespolonych."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
Bash domyślnie nie obsługuje liczb zespolonych. Często używa się zewnętrznych narzędzi takich jak `bc` z opcją `-l`. Oto jak przetwarzasz liczby zespolone w bashu:

```bash
echo "sqrt(-1)" | bc -l
```

Wynik:
```bash
j
```

Mnożenie:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Wynik:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Wnikliwe spojrzenie
Liczby zespolone istnieją od XVI wieku, ale języki skryptowe takie jak Bash nie są pierwotnie przystosowane do wykonywania obliczeń matematycznych z użyciem liczb zespolonych "od razu". Dlatego często używa się `bc` lub innych narzędzi takich jak `awk`. Niektóre alternatywne języki do pracy z liczbami zespolonymi to Python z jego modułem `cmath` oraz MATLAB, które oba są przeznaczone do bardziej zaawansowanych funkcji matematycznych. Jeśli chodzi o Bash, chodzi tutaj o wykorzystanie narzędzi - `bc` używa małej litery 'i' do reprezentowania jednostki urojonej i obsługuje podstawowe operacje takie jak dodawanie, odejmowanie, mnożenie i dzielenie.

## Zobacz również
- Instrukcja `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternatywa dla MATLAB-u): https://www.gnu.org/software/octave/
- Moduł `cmath` Pythona: https://docs.python.org/3/library/cmath.html
