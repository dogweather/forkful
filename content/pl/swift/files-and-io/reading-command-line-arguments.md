---
date: 2024-01-20 17:56:59.743955-07:00
description: "Argumenty linii polece\u0144 to parametry przekazywane do programu,\
  \ kiedy jest on uruchamiany z terminala. Programi\u015Bci korzystaj\u0105 z nich,\
  \ by wp\u0142ywa\u0107 na\u2026"
lastmod: '2024-03-11T00:14:08.976808-06:00'
model: gpt-4-1106-preview
summary: "Argumenty linii polece\u0144 to parametry przekazywane do programu, kiedy\
  \ jest on uruchamiany z terminala. Programi\u015Bci korzystaj\u0105 z nich, by wp\u0142\
  ywa\u0107 na\u2026"
title: "Odczytywanie argument\xF3w linii polece\u0144"
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
