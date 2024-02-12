---
title:                "Sprawdzanie, czy katalog istnieje"
aliases: - /pl/bash/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:50.545586-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

W programowaniu Bash sprawdzanie, czy katalog istnieje, jest kluczowym mechanizmem kontrolnym wykorzystywanym do weryfikacji obecności katalogu przed wykonaniem operacji na plikach. Ta kontrola jest kluczowa, aby uniknąć błędów takich jak próba dostępu lub modyfikacji katalogów, które nie istnieją, zapewniając sprawniejsze i bardziej przewidywalne wykonanie skryptu.

## Jak to zrobić:

W swojej istocie Bash pozwala na sprawdzenie istnienia katalogu za pomocą instrukcji warunkowych i operatora `-d`. Poniżej znajduje się prosty przykład demonstrujący, jak wykonać to sprawdzenie.

```bash
if [ -d "/ścieżka/do/katalogu" ]; then
    echo "Katalog istnieje."
else
    echo "Katalog nie istnieje."
fi
```

Przykładowe wyjście (jeśli katalog istnieje):
```
Katalog istnieje.
```

Przykładowe wyjście (jeśli katalog nie istnieje):
```
Katalog nie istnieje.
```

W bardziej złożonych skryptach często łączy się tę kontrolę z innymi operacjami, takimi jak tworzenie katalogu, jeśli nie istnieje:

```bash
DIR="/ścieżka/do/katalogu"
if [ -d "$DIR" ]; then
    echo "$DIR istnieje."
else
    echo "$DIR nie istnieje. Tworzenie teraz..."
    mkdir -p "$DIR"
    echo "$DIR został utworzony."
fi
```

Przykładowe wyjście (jeśli katalog nie istnieje, a następnie zostaje utworzony):
```
/ścieżka/do/katalogu nie istnieje. Tworzenie teraz...
/ścieżka/do/katalogu został utworzony.
```

Chociaż Bash sam w sobie dostarcza rozbudowanych narzędzi do takich kontroli, nie ma popularnych bibliotek stron trzecich specjalnie dla tego zadania, ponieważ native komendy Bash są w pełni zdolne i efektywne do weryfikacji obecności katalogu.
