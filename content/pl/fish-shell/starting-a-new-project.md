---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Rozpoczęcie nowego projektu to jak budowla od podstaw - jest to start punkt do kreowania nowego oprogramowania. Programiści robią to, aby rozwiązać nowe problemy lub by poprawić istniejący proces.

## Jak to zrobić:
Zaczynamy od instalacji Fish Shella, jeśli jeszcze go nie masz. Najprostszy sposób to:
```Fish Shell
brew install fish
```
Załóżmy, że chcemy stworzyć nowy projekt o nazwie "MyProject". Tworzymy nowy katalog i przechodzimy do niego:
```Fish Shell
mkdir MyProject
cd MyProject
```
Zgodnie z dobrą praktyką, powinieneś zainicjować git w swoim projekcie:
```Fish Shell
git init
```
I to wszystko! Rozpoczęliśmy nowy projekt.

## Głębsza wiedza
Historiczenie, programiści używali różnych shelli do pisania skryptów takich jak Bash czy zsh. Fish jest jednak nowocześniejszy, łatwiejszy w użyciu, i posiada wiele przydatnych funkcji które przyspieszają proces programowania.

Co do alternatyw, mamy wiele innych opcji jak PowerShell, C Shell, Korn Shell i wiele innych. Każdy ma swoje unikalne funkcje i możesz wybrać ten, który najlepiej odpowiada Twoim potrzebom.

Gdy tworzysz nowy projekt, duże znaczenie ma struktura katalogów i nazwy plików. Ważne jest, aby były one odpowiednie dla Twojego projektu i zgodne z ogólnie przyjętymi standardami.

## Zobacz też
1. Dokumentacja Fish Shell - https://fishshell.com/docs/current/index.html
2. Porównanie różnych shelli - https://www.slant.co/topics/514/~best-unix-shells
3. Instrukcja jak używać git - https://git-scm.com/doc
4. Poradnik organizacji projektów - https://docs.python-guide.org/writing/structure/