---
date: 2024-01-26 04:11:30.109259-07:00
description: "Jak to zrobi\u0107: W Bashu, twoje terminal to w zasadzie REPL. Wpisujesz\
  \ polecenie; czyta je, ocenia, wypisuje wynik, i zaczyna od nowa czekaj\u0105c na\
  \ twoje\u2026"
lastmod: '2024-03-13T22:44:35.586540-06:00'
model: gpt-4-0125-preview
summary: W Bashu, twoje terminal to w zasadzie REPL.
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
W Bashu, twoje terminal to w zasadzie REPL. Wpisujesz polecenie; czyta je, ocenia, wypisuje wynik, i zaczyna od nowa czekając na twoje kolejne polecenie. Oto przykład użycia Bash jako REPL:

```Bash
$ echo "Witaj, świecie!"
Witaj, świecie!
$ x=$((6 * 7))
$ echo $x
42
```

Twoje wejście podąża za znakiem `$ `, a wynik jest wyświetlany w następnej linii. Proste, prawda?

## W głąb
Bash, skrót od Bourne Again SHell, jest domyślną powłoką w wielu systemach opartych na Unixie. Jest to ulepszona wersja oryginalnej powłoki Bourne, stworzona pod koniec lat 70. Choć Bash jest potężnym narzędziem skryptowym, jego tryb interaktywny pozwala na wykonywanie poleceń linia po linii.

Biorąc pod uwagę alternatywy, masz do dyspozycji REPL Pythona (wystarczy wpisać `python` w terminalu), Node.js (z `node`) oraz IPython, ulepszoną interaktywną powłokę Pythona. Każdy język ma zazwyczaj swoją własną implementację REPL.

Pod spodem REPL to pętle, które analizują twoje wejście (polecenia lub kod), wykonują je, i zwracają wynik do stdout (twojego ekranu), często używając bezpośrednio interpretera języka. Ta natychmiastowość informacji zwrotnej jest świetna do nauki i prototypowania.

## Zobacz także
- [Oficjalna dokumentacja GNU Bash](https://gnu.org/software/bash/manual/bash.html)
- [Naucz się Shell - interaktywny samouczek](https://www.learnshell.org/)
- [Oficjalna strona IPython](https://ipython.org/)
- [REPL.it](https://replit.com/): Wielojęzyczny online REPL (Nie tylko Bash!)
