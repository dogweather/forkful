---
title:                "Swift: Odczytywanie argumentów linii poleceń"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w języku Swift może być fascynującym wyzwaniem, ale na początku może wydawać się dość trudne. Jednak nauka używania podstawowych narzędzi, takich jak parametry wiersza poleceń, może pomóc Ci zrozumieć jak działa ten język i ułatwić Ci pracę w przyszłości.

## Jak to zrobić

Aby przeczytać parametry wiersza poleceń w języku Swift, możesz użyć metody `arguments` klasy `Process`. Najpierw musisz utworzyć obiekt typu `Process`, a następnie użyć metody `arguments` aby uzyskać listę podanych parametrów. Poniższy kod jest prostym przykładem, który pokazuje jak to zrobić:

```Swift
let process = Process()
let arguments = process.arguments

print(arguments)
```

Gdy uruchomisz ten kod, powinnaś zobaczyć listę parametrów, które zostały podane przy uruchamianiu programu w terminalu. Na przykład, jeśli uruchomiłaś program ze wiersza poleceń wpisując `swift Example.swift 1 2 3`, lista parametrów będzie wyglądała tak:

```
["Example.swift", "1", "2", "3"]
```

## Głębszy zanurzenie

Możesz również użyć parametrów wiersza poleceń do wykonywania różnych działań w swoim programie. Na przykład, możesz użyć prostego warunku `if` aby sprawdzić czy został podany odpowiedni parametr. Poniższy kod pokazuje jak to zrobić:

```Swift
if arguments.contains("help") {
    print("Ta aplikacja zawiera pomoc.")
}
```

W tym przykładzie, jeśli podałaś jako parametr `help`, wyświetli się odpowiednie komunikat w konsoli.

## Zobacz również

- [Dokumentacja Swift: Process](https://developer.apple.com/documentation/foundation/process)
- [Inne sposoby czytania parametrów wiersza poleceń w Swift](https://stackoverflow.com/questions/37540209/swift-how-i-do-reading-a-command-line-argument/37540286#37540286)
- [Tutorial: Parametry wiersza poleceń w języku Swift](https://www.raywenderlich.com/287-command-line-programs-on-macos-tutorial)