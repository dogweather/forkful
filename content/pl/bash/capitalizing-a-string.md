---
title:                "Zmiana wielkości liter w ciągu znaków"
html_title:           "Bash: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego? 
"Marcus" lub "MARCUS"? Czy ty też masz trudności z tym wielkością liter w słowach? Programiści również mają. Dlatego w tym artykule dowiesz się jak zmienić wielkość liter w ciągach tekstu w Bash i dlaczego jest to ważne w pracy programisty.

## Jak to zrobić:
W Bash jest wiele sposobów na zmianę wielkości liter w ciągu tekstu. Poniżej przedstawiamy kilka przykładowych rozwiązań, wraz z kodem i oczekiwanym wynikiem.

```Bash
echo "MARCUS" | tr '[:upper:]' '[:lower:]'
```
Output: marcus

```Bash
echo "MARCUS" | awk '{print tolower($0)}'
```
Output: marcus

```Bash
string="MARCUS"
echo "${string,,}"
```
Output: marcus

## Głębsze kwestie:
Zmiana wielkości liter w ciągach tekstowych jest czymś, czego programiści potrzebują od początków tworzenia komputerów. Dawniej zwykło się używać dziurkowanych kartek, a edycja tekstu na nich była uciążliwa, dlatego automatyczna zmiana wielkości liter była dużym ułatwieniem. Alternatywnymi sposobami na zmianę wielkości liter są na przykład użycie funkcji w innych językach programowania, takich jak Python czy JavaScript. W Bash można również zmienić wielkość pierwszej litery w słowie, korzystając z polecenia ```${string^}```.

## Zobacz także:
Jeśli chcesz dowiedzieć się więcej o zmianie wielkości liter w Bash, polecamy przeczytać ten artykuł na stronie [Shell Scripting Tutorial](https://www.shellscript.sh/case.html). Jeśli z kolei chcesz poznać inne polecenia i funkcje Bash, zajrzyj na stronę [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html).