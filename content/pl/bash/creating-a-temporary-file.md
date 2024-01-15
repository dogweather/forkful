---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Bash: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

### Dlaczego

Tworzenie plików tymczasowych jest częstą operacją w skrypcie Bash. Często potrzebujemy tymczasowo przechować dane lub wykonać operacje na plikach, a potem usunąć je po zakończeniu zadania. Stworzenie tymczasowego pliku jest szybkim i prostym sposobem na to.

### Jak to zrobić

```Bash
# Utworzenie tymczasowego pliku o nazwie "temp.txt"
touch temp.txt
# Wyświetlenie zawartości pliku
cat temp.txt
# Zapisanie tekstu do pliku
echo "To jest przykładowy tekst" > temp.txt
# Wyświetlenie zawartości pliku
cat temp.txt
# Usunięcie pliku
rm temp.txt
```
```
To jest przykładowy tekst
```

### Głębsza analiza

Tworzenie tymczasowego pliku w Bash jest bardzo proste - możemy użyć do tego polecenia `touch`, które tworzy pusty plik o podanej nazwie. Możemy także użyć `echo` do zapisania tekstu lub danych do naszego pliku tymczasowego.

Warto zauważyć, że domyślnie plik tymczasowy jest tworzony w bieżącym katalogu roboczym, ale możemy także podać pełną ścieżkę do pliku, jeśli chcemy go stworzyć w innym miejscu.

Jedną z najważniejszych rzeczy, na które warto zwrócić uwagę, jest to, że plik tymczasowy powinien zostać usunięty po zakończeniu zadania. W przeciwnym razie może zajmować niepotrzebną przestrzeń na naszym dysku. W powyższym przykładzie użyliśmy polecenia `rm` do usunięcia pliku, ale zawsze możemy także ręcznie usunąć go za pomocą ulubionego menedżera plików.

### Zobacz również

- [Dokumentacja Bash na stronie GNU](https://www.gnu.org/software/bash/manual/)
- [5 prostych trików, które ułatwią Ci pracę w Bash](https://thoughtbot.com/blog/five-awesome-bash-tricks)
- [Ważne elementy w skrypcie Bash](https://www.linux.com/learn/bash-scripting-important-elements-bash)