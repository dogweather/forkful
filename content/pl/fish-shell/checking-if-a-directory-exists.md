---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:56:10.675936-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Sprawdzanie, czy katalog istnieje, to proces weryfikacji obecności katalogu w systemie plików. Programiści robią to, aby uniknąć błędów podczas prób odczytu, zapisu czy tworzenia plików w nieistniejącym katalogu.

## How to: (Jak to zrobić:)
Sprawdzanie, czy katalog istnieje w Fish, jest proste. Wykorzystaj `test` i `and/or` do kontrolowania przepływu.

```Fish Shell
if test -d /jakis/katalog
    echo "Katalog istnieje!"
else
    echo "Katalog nie istnieje."
end
```

Jeśli katalog `/jakis/katalog` istnieje, zobaczysz:
```
Katalog istnieje!
```
A jeśli nie, to:
```
Katalog nie istnieje.
```

## Deep Dive (Dogłębna analiza)
Sprawdzanie istnienia katalogu nie jest niczym nowym; od lat jest to standardowa funkcjonalność w skryptach powłoki. W Unixowych systemach, jak Linux czy macOS, komenda `test -d` skutecznie sprawdza istnienie katalogu.

Alternatywnie, można użyć `and` i `or` dla krótszej wersji:

```Fish Shell
test -d /jakis/katalog; and echo "Katalog istnieje"; or echo "Katalog nie istnieje"
```

Ta metoda jest bardziej bezpośrednia i nie wymaga `if`. `test -d` zwraca `0` (prawda) gdy katalog istnieje i niezerowy kod błędu (fałsz) w przeciwnym razie.

W starszych powłokach, jak Bash, często stosuje się `[]` lub `[[ ]]` do podobnych zadań, ale Fish stosuje `test` dla zwiększenia czytelności i unifikacji. Fish z założenia unika nadmiarowych nawiasów gdzie to możliwe, dążąc do większej klarowności i prostoty.

## See Also (Zobacz również)
- [Fish Documentation](https://fishshell.com/docs/current/index.html) – Dokumentacja Fish, pełna informacji o wbudowanych funkcjach i składni.
- [Unix test man page](https://man7.org/linux/man-pages/man1/test.1.html) – Dokumentacja komendy `test` w systemach Unix.
- [The Linux Command Line](http://linuxcommand.org/) – Przewodnik po komendach Linuxa, który przydaje się do porównań z Fish.