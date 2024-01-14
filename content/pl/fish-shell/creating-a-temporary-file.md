---
title:    "Fish Shell: Tworzenie pliku tymczasowego"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

 Tworzenie tymczasowych plików jest powszechną praktyką w programowaniu, szczególnie w powłokach systemowych. Pozwala ono na przechowywanie danych tymczasowo, bez konieczności trwałego zapisu, co jest szczególnie przydatne w przypadku skryptów automatyzujących różne zadania.

## Jak to zrobić

Tworzenie tymczasowych plików w powłoce Fish jest bardzo proste. Wystarczy skorzystać z wbudowanej funkcji `mktemp`, która automatycznie generuje unikalną nazwę dla tymczasowego pliku. W poniższym przykładzie zapiszemy kilka danych do tymczasowego pliku i wyświetlimy jego zawartość:

```Fish Shell
set tmpfile (mktemp)
echo "To jest przykładowy tekst" > $tmpfile
cat $tmpfile
```

Kiedy wykonamy ten kod, otrzymamy następujący wynik:

```
To jest przykładowy tekst
```

Teraz możemy bezpiecznie używać danych przechowywanych w tymczasowym pliku, a po zakończeniu działania skryptu, plik zostanie automatycznie usunięty.

## Pogłębione informacje

Funkcja `mktemp` w Fish Shell wykorzystuje polecenie systemowe `mktemp` z systemu Unix, które generuje pliki tymczasowe w katalogu `/tmp`. Możemy również podać własną ścieżkę do folderu, w którym chcemy utworzyć tymczasowy plik, poprzez przekazanie opcji `-p` do funkcji `mktemp`.

Ponadto, w Fish Shell istnieje również funkcja `mktmp` która pozwala na jeszcze prostsze tworzenie tymczasowych plików. Wystarczy podać prefix dla nazwy pliku, a funkcja automatycznie dodat znaki losowe, aby uniknąć nadpisania istniejących plików.

## Zobacz również

- Dokumentacja Fish Shell: [Tworzenie tymczasowych plików](https://fishshell.com/docs/current/cmds/mktemp.html)
- Poradnik po polsku: [Tworzenie i usuwanie plików tymczasowych w Fish Shell](https://blog.kamilogorek.pl/tworzenie-i-usuwanie-plikow-tymczasowych-w-fish-shell/)
- Inne przydatne sposoby na korzystanie z plików tymczasowych w programowaniu.