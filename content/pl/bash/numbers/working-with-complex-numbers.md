---
date: 2024-01-26 04:37:06.709668-07:00
description: "Liczby zespolone sk\u0142adaj\u0105 si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ i cz\u0119\u015Bci urojonej. Programi\u015Bci u\u017Cywaj\u0105 ich w dziedzinach\
  \ takich jak przetwarzanie sygna\u0142\xF3w, mechanika\u2026"
lastmod: 2024-02-19 22:04:54.715731
model: gpt-4-0125-preview
summary: "Liczby zespolone sk\u0142adaj\u0105 si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ i cz\u0119\u015Bci urojonej. Programi\u015Bci u\u017Cywaj\u0105 ich w dziedzinach\
  \ takich jak przetwarzanie sygna\u0142\xF3w, mechanika\u2026"
title: Praca z liczbami zespolonymi
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone składają się z części rzeczywistej i części urojonej. Programiści używają ich w dziedzinach takich jak przetwarzanie sygnałów, mechanika kwantowa oraz wszędzie tam, gdzie wymagane są obliczenia ponad zwykłe liczby rzeczywiste, ponieważ normalne liczby rzeczywiste po prostu nie wystarczają.

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
