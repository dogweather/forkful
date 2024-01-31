---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-19
html_title:           "Bash: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Co i po co? Sprawdzanie istnienia katalogu to zapytanie w Bashu, aby zobaczyć, czy dany folder jest na dysku. Robimy to, by zapobiec błędom przy próbie dostępu do nieistniejących katalogów.

## How to:
Jak to zrobić:

```Bash
if [ -d "$DIRECTORY" ]; then
  echo "Katalog $DIRECTORY istnieje."
else
  echo "Katalog $DIRECTORY nie istnieje."
fi
```

Wyjście przykładowe, jeśli katalog istnieje:

```
Katalog /home/uzytkownik/Dokumenty istnieje.
```

Wyjście przykładowe, jeśli katalog nie istnieje:

```
Katalog /home/uzytkownik/NieistniejacyKatalog nie istnieje.
```

## Deep Dive:
Zanurzenie:

Historia: Sprawdzanie istnienia pliku/katalogu jest jedną z podstawowych operacji w Unixowych systemach od początków ich istnienia. Bash, będąc jedną z najpopularniejszych powłok w systemach Unix i Linux, odziedziczył wiele funkcji z wcześniejszych powłok sh i ksh.

Alternatywy: Oprócz wbudowanej instrukcji `[ -d "$DIRECTORY" ]`, możemy użyć `[[ -d "$DIRECTORY" ]]` dla nowszych wersji Bash lub `test -d "$DIRECTORY"` w starych skryptach. Również polecenie `find` i `ls` mogą służyć do identyfikacji istnienia katalogów, ale są to bardziej rozbudowane narzędzia niż potrzeba w prostym scenariuszu sprawdzania istnienia.

Szczegóły: Flaga `-d` w [ ] sprawdza, czy określona ścieżka jest katalogiem. Jest to bezpieczne narzędzie, które można stosować w skryptach wykonywanych na różnych komputerach i w różnych środowiskach, ponieważ rzadko ulega zmianom między wersjami Bash.

## See Also:
Zobacz również:

- `man test` - instrukcja działania polecenia test w systemach Unix
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/ - kompendium wiedzy o skryptach w Bashu
- Stack Overflow: https://stackoverflow.com/ - społeczność programistów, z której pomocą można rozwiązywać trudne problemy i zadawać pytania

Pamiętaj, że Internet jest Twoim przyjacielem. Kiedy napotykasz na problem lub potrzebujesz więcej informacji, zawsze jest gdzieś strona lub forum, które może pomóc!
