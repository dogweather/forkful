---
aliases:
- /pl/bash/using-an-interactive-shell-repl/
date: 2024-01-26 04:11:30.109259-07:00
description: "REPL to skr\xF3t od Read-Eval-Print Loop, prostego, interaktywnego \u015B\
  rodowiska programistycznego. Programi\u015Bci u\u017Cywaj\u0105 go do szybkiego\
  \ pisania i testowania\u2026"
lastmod: 2024-02-18 23:08:49.786102
model: gpt-4-0125-preview
summary: "REPL to skr\xF3t od Read-Eval-Print Loop, prostego, interaktywnego \u015B\
  rodowiska programistycznego. Programi\u015Bci u\u017Cywaj\u0105 go do szybkiego\
  \ pisania i testowania\u2026"
title: Korzystanie z interaktywnego shella (REPL)
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL to skrót od Read-Eval-Print Loop, prostego, interaktywnego środowiska programistycznego. Programiści używają go do szybkiego pisania i testowania kodu, eksperymentowania ze składnią oraz nauki koncepcji programowania bez konieczności tworzenia i uruchamiania całych aplikacji.

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
