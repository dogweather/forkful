---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zamiana liter na wielkie w stringu oznacza, że zmieniamy wszystkie litery na ich wielkie odpowiedniki, tzw. kapitalizację. Programiści robią to, by ujednolicić tekst, np. w tytułach, nazwach własnych albo na początku zdania.

## Jak to zrobić:
```Bash
# Prosty sposób na zamianę na wielkie litery
text="witaj, świecie"
capitalized_text=$(echo "$text" | tr '[:lower:]' '[:upper:]')
echo $capitalized_text
```
Wyjście:
```
WITAJ, ŚWIECIE
```

## Zanurzmy się głębiej
Kapitalizacja stringów w Bashu to nic nowego. Już wcześniejsze wersje Unix i jego potomków oferowały narzędzia jak `tr`, `awk`, czy `sed`, do manipulacji tekstem. `tr` jest szybkie i proste, ale alternatywy jak `awk` pozwalają na skomplikowane przetwarzanie w skrypcie. Co więcej, w nowych wersjach Bash (4.0+) można użyć wbudowanych funkcji do manipulacji stringami, np. `${text^^}` by zamienić na wielkie litery, gdzie `text` to zmienna zawierająca stringa. To efektywniejsze, bo unikasz wywołania zewnętrznych komend.

Alternatywnie:
```Bash
# Używanie wbudowanego mechanizmu Bash do kapitalizacji
text="witaj, świecie"
echo "${text^^}"
```
Wyjście jest takie samo:
```
WITAJ, ŚWIECIE
```

## Zobacz także
- Bash manual: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- Ogólna dokumentacja GNU `tr`: [https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
