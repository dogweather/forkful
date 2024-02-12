---
title:                "Odczytywanie argumentów linii poleceń"
aliases:
- pl/swift/reading-command-line-arguments.md
date:                  2024-01-20T17:56:59.743955-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Argumenty linii poleceń to parametry przekazywane do programu, kiedy jest on uruchamiany z terminala. Programiści korzystają z nich, by wpływać na zachowanie programu bez zmiany kodu.

## How to:
Odczytujemy argumenty z tablicy `CommandLine.arguments`. Przykładowy kod:

```Swift
// main.swift

for arg in CommandLine.arguments {
    print(arg)
}
```

Jeśli uruchomisz program z dodatkowymi parametrami, na przykład:

```bash
swift run myprogram arg1 arg2
```

Wynik będzie następujący:

```
/path/to/myprogram
arg1
arg2
```

## Deep Dive:
Argumenty linii poleceń są starym, lecz uniwersalnym pomysłem interakcji użytkownika z programem. Alternatywy to interfejsy graficzne czy konfiguracje z plików, lecz CLI (Command Line Interface) jest często wybierany dla uprośczenia i automatyzacji. W Swift `CommandLine` jest obiektem dostarczającym dostęp do argumentów. Implementacja zakłada, że pierwszym argumentem jest ścieżka do wykonywalnego pliku programu, a reszta to przekazywane parametry.

## See Also:
- [Swift.org Documentation](https://www.swift.org/documentation/): Oficjalna dokumentacja Swift.
- [Ray Wenderlich: Command Line Programs on macOS Tutorial](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial): Tutorial o programowaniu wiersza poleceń na macOS.
