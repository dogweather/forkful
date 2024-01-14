---
title:                "Swift: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak kod programu może odczytywać informacje wprowadzone przez użytkownika w wierszu poleceń? W tym artykule dowiesz się, dlaczego umiejętność czytania argumentów wiersza poleceń jest przydatna w programowaniu w języku Swift.

## Jak to zrobić

Odczytywanie argumentów wiersza poleceń w Swift jest łatwe i wymaga tylko kilku kroków. Najpierw należy zaimportować moduł `Foundation`, który zawiera potrzebne nam funkcje, a następnie użyć metody `arguments` na obiekcie `Process`. Spójrzmy na przykład, gdzie nasz program odczytuje argumenty wiersza poleceń i wypisuje je na ekran:

```Swift
import Foundation

let arguments = ProcessInfo.processInfo.arguments

print("Wprowadzone argumenty: \(arguments)")
```

Dla przykładu, wywołując nasz program z argumentami "Swift", "programowanie" i "blog", otrzymamy następujący output:

```bash
$ ./program argumenty Swift programowanie blog
Wprowadzone argumenty: ["Swift", "programowanie", "blog"]
```

Łatwe, prawda? Teraz możesz wykorzystać te argumenty do różnych celów w swoim programie.

## Deep Dive

Ok, ale jak to właściwie działa? Gdy uruchamiamy program z argumentami, system operacyjny przekazuje je do jądra systemu. Następnie jądro przekazuje je do procesu, w którym uruchamia się nasz program. Kolejno, nasz program odczytuje te argumenty za pomocą funkcji `arguments`. Proste i skuteczne!

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o odczytywaniu argumentów wiersza poleceń w języku Swift, zapoznaj się z dokumentacją Apple lub z poniższymi artykułami:

- [Reading Command Line Arguments in Swift](https://medium.com/@MaxDesiatov/reading-command-line-arguments-in-swift-3-0-a54c27e7d2a3)
- [Working with Command-Line Arguments in Swift](https://www.raywenderlich.com/382-command-line-programs-on-macos-tutorial-getting-started-with-swift)
- [ArgumentParser - parse command-line arguments with Swift](https://github.com/apple/swift-argument-parser)