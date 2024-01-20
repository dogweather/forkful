---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja ciągu do małych liter oznacza zamianę wszystkich wielkich liter znajdujących się w danym ciągu na małe. Programiści robią to, by ułatwić porównywanie teksu, sortowanie, a także by upewnić się, że dane są spójne i normalizowane.

## Jak to zrobić:

W Bashu istnieje wiele sposobów na konwersję ciągu do małych liter. Poniżej znajduje się kilka przykładów:

```Bash
# Metoda 1: za pomocą komendy tr
echo "HELLO, WORLD" | tr '[:upper:]' '[:lower:]'

# Metoda 2: korzystając z wbudowanej funkcji bash
ciag="HELLO, WORLD"
echo "${ciag,,}"

# Metoda 3: za pomocą komendy awk
echo "HELLO, WORLD" | awk '{print tolower($0)}'
```

Wszystkie te metody zwrócą identyczny rezultat, czyli "hello, world".

## Szerzej:

1. **Kontekst historyczny**: Wiele języków programowania, w tym Bash, dysponuje funkcją konwersji ciągów na małe litery. Funkcjonalność ta jest używana od czasów, gdy zaczynaliśmy rozwijać skomplikowane algoritmy na tekście.
2. **Alternatywy**: Inną metodą jest użycie Perl, Python, lub sed do konwersji ciągów na małe litery.
3. **Szczegóły implementacji**: Wszystkie prezentowane metody iterują po każdym znaku w ciągu i zamieniają litery na małe. Przebieg działania różni się trochę, ale idea jest taka sama: porównaj każdą literę ze swoim małym odpowiednikiem i dokonaj zamiany, jeśli jest to konieczne.

## Zobacz również:

- Strona man dla [tr](https://man7.org/linux/man-pages/man1/tr.1.html), [awk](https://man7.org/linux/man-pages/man1/awk.1p.html) i [bash](https://man7.org/linux/man-pages/man1/bash.1.html) daje więcej opcji i przykładów użycia tych poleceń.
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/) zawiera wiele informacji o skryptach Bash, w tym o operacji na ciągach.
- [Bash String Manipulation](https://linuxconfig.org/bash-string-manipulation) jest świetnym zasobem, jeśli chcesz dowiedzieć się więcej o manipulacji ciągami w Bashu.