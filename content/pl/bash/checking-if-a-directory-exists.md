---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Bash: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje, to metoda używana w Bash, aby potwierdzić, czy dany katalog faktycznie istnieje na dysku. Programiści robią to unikania błędów podczas prób zapisu lub odczytu z niewłaściwych lokalizacji.

## Jak to zrobić:

Przykład kodu, który sprawdza, czy katalog istnieje, możemy napisać tak:

```Bash
if [ -d "$nazwa_katalogu" ]; then
    echo "Katalog istnieje"
else 
    echo "Katalog nie istnieje"
fi
```

Gdy `$nazwa_katalogu` to ścieżka do sprawdzenia, to na wyjściu otrzymamy "Katalog istnieje" albo "Katalog nie istnieje" w zależności od tego, czy dany katalog rzeczywiście istnieje.

## Głębszy wgląd

Koncepty sprawdzania istnienia katalogu są jednymi z najważniejszych w Bash i są obecne prawie od początku języka. Alternatywą dla powyższego kodu może być użycie `if` z `test -d "$nazwa_katalogu"`. Wewnętrznie, Bash wykonuje te operacje poprzez sprawdzenie informacji inode systemu plików na dysku. 

## Zobacz także

- Inne flagi do testowania rodzaju pliku w Bash: [GNU Bash Manual](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- Plik inode i jak Bash go używa: [Understanding Inodes](https://opensource.com/article/19/3/what-inode)
- Więcej o poleceniu `test`: [GNU Coreutils Manual](https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html)