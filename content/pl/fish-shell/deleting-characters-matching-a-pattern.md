---
title:                "Fish Shell: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Dlaczego warto usunąć znaki pasujące do wzoru w Fish Shell?

Często zdarza się, że w trakcie pisania kodu w terminalu używamy polecenia `grep` do wyszukiwania tekstu. Jednak co zrobić, gdy chcemy usunąć wszystkie znaki pasujące do danego wzoru? W takim przypadku przyda nam się opcja `--option` w Fish Shell, która pozwala na usunięcie wszystkich pasujących znaków i zachowanie tylko tych, które nam są potrzebne. Dzięki tej opcji możemy szybko i sprawnie przetwarzać nasze dane.

# Jak tego dokonać w Fish Shell?

Aby usunąć znaki pasujące do danego wzoru w Fish Shell, należy użyć polecenia `grep` wraz z opcją `--option`. Przykładowe użycie wyglądałoby następująco:

```Fish Shell
grep -v --option "szukany wzór" plik.txt
```

Wykonując to polecenie, program wyświetli wszystkie linie z pliku `plik.txt`, które nie zawierają szukanego wzoru. W przypadku, gdy chcemy zastąpić znaki pasujące do wzoru innym tekstem, możemy użyć opcji `--replace` wraz z danym wzorem.

# Głębsza analiza

Opcja `--option` jest bardzo przydatna, jednak warto pamiętać, że polecenia należy uważnie dobierać, aby uniknąć przypadkowego usunięcia potrzebnych danych. W przypadku, gdy identyczność znaków ma znaczenie, należy zastosować dokładniejsze wzorce, np. wykorzystując [wyrażenia regularne](https://en.wikipedia.org/wiki/Regular_expression).

# Zobacz także

* [Dokumentacja Fish Shell](https://fishshell.com/docs/current/cmds/grep.html)
* [Poradnik odnośnie wyrażeń regularnych w Fish Shell](https://ryanstutorials.net/linuxtutorial/grep.php)