---
date: 2024-01-20 17:56:59.743955-07:00
description: "How to: Odczytujemy argumenty z tablicy `CommandLine.arguments`. Przyk\u0142\
  adowy kod."
lastmod: '2024-03-13T22:44:35.770081-06:00'
model: gpt-4-1106-preview
summary: Odczytujemy argumenty z tablicy `CommandLine.arguments`.
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

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
