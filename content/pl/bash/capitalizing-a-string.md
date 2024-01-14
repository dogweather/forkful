---
title:                "Bash: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek próbowałeś zmienić styl tekstu w swoim skrypcie Bash? Może chcesz, aby wszystkie litery w zdaniu były pisane wielkimi literami lub tylko pierwsza litera? W tym artykule dowiesz się, jak prosto zmienić styl tekstu w Bash, aby dopasować go do swoich potrzeb.

## Jak To Zrobić

Aby zmienić styl tekstu w Bash, musisz użyć polecenia "tr". Polecenie to służy do zmiany lub usuwania znaków w tekście. Aby zmienić styl tekstu, musimy wybrać odpowiednie opcje polecenia "tr".

```Bash
echo "witaj w świecie Bash" | tr '[:lower:]' '[:upper:]'
```
W powyższym przykładzie, polecenie "echo" służy do wyświetlenia tekstowego wyjścia, a następnie przesyła je do polecenia "tr". Opcja '[:lower:]' informuje "tr", aby zmienił wszystkie litery w tekście na małe litery, a opcja '[:upper:]' zmienia litery na wielkie. Wynik tego polecenia będzie wyglądał następująco:
"WITAJ W ŚWIECIE BASH"

Możesz również użyć opcji "[:upper:]" bezpośrednio w poleceniu "echo", aby uniknąć przesyłania tekstu do polecenia "tr".

```Bash
echo "witaj w świecie Bash" | tr '[:lower:]' '[:upper:]'
```

## Deep Dive

Aby lepiej zrozumieć jak działa polecenie "tr", musimy poznać jego strukturę. Polecenie składa się z dwóch części, pierwszą jest lista znaków do zmiany, a drugą lista odpowiadających im znaków lub opcji. Możesz również użyć opcji "-d" w drugiej części, aby usunąć wybrane znaki z tekstu zamiast je zmieniać. Opcje mogą być również kompilowane, co oznacza, że można użyć kilku opcji jednocześnie.

## Zobacz też
- Oficjalna dokumentacja polecenia "tr": [https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- Tutorial o poleceniu "tr" w Bash: [https://www.linux.com/learn/using-tr-translate-or-delete-characters-bash](https://www.linux.com/learn/using-tr-translate-or-delete-characters-bash)
- Przydatne komendy Bash: [https://www.linux.com/topic/desktop/useful-bash-commands/](https://www.linux.com/topic/desktop/useful-bash-commands/)