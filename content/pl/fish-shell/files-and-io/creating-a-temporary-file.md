---
date: 2024-01-20 17:40:12.918048-07:00
description: "How to: W Fish mo\u017Cna \u0142atwo stworzy\u0107 plik tymczasowy.\
  \ U\u017Cyj `mktemp`, by zadzia\u0142a\u0107 magi\u0119."
lastmod: '2024-03-13T22:44:35.861147-06:00'
model: gpt-4-1106-preview
summary: "W Fish mo\u017Cna \u0142atwo stworzy\u0107 plik tymczasowy."
title: Tworzenie pliku tymczasowego
weight: 21
---

## How to:
W Fish można łatwo stworzyć plik tymczasowy. Użyj `mktemp`, by zadziałać magię:

```Fish Shell
set temp_file (mktemp)
echo "Temporary data" > $temp_file
cat $temp_file
# Output: Temporary data
```

Po zakończeniu, nie zapomnij posprzątać:

```Fish Shell
rm $temp_file
```

## Deep Dive
Tworzenie plików tymczasowych ma długą historię w *nix-owych systemach operacyjnych. Tradycyjnie używa się `/tmp` jako katalogu tymczasowego. W Fish, `mktemp` to swoista fasada przed tradycyjną komendą systemową, zapewniając jednak łatwiejsze, bezpieczniejsze API. Inne obsługi plików tymczasowych to `tempfile` czy `tmpfile()` w różnych środowiskach programistycznych, ale ich dostępność może się różnić.

Zaletą korzystania z `mktemp` jest to, że tworzy ono unikalne nazwy plików, zmniejszając ryzyko konfliktów i potencjalnych problemów z bezpieczeństwem.

## See Also
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html#syntax) - dla dogłębniej znajomości składni Fish.
- [mktemp Man Page](https://linux.die.net/man/1/mktemp) - dla technicznych szczegółów polecenia `mktemp`.
- [Wiki UNIX /tmp Directory](https://en.wikipedia.org/wiki//tmp) - więcej o roli katalogu `/tmp` w systemach podobnych do Unix.
