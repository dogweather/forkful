---
title:    "Swift: Odczytywanie argumentów wiersza poleceń"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego?

Czy kiedykolwiek zastanawiałeś się, jak programy takie jak systemy operacyjne czy aplikacje konsolowe przyjmują argumenty z wiersza poleceń? W tym wpisie dowiecie się, dlaczego jest to ważna umiejętność dla każdego programisty Swift.

## Jak To Zrobić?

Aby odczytać argumenty z wiersza poleceń w Swift, użyjemy biblioteki "Foundation" i funkcji "CommandLine.arguments". Poniższy kod demonstruje, jak możemy wyświetlić wszystkie argumenty przekazane do programu:

```Swift
import Foundation

let arguments = CommandLine.arguments

for argument in arguments {
    print(argument)
}
```

Aby przekazać argumenty do programu podczas kompilacji, wystarczy wpisać je po nazwie pliku w terminalu. Przykładowo, jeśli nazwa pliku to "myProgram.swift" oraz chcemy przekazać argumenty "Hello" i "world", to wpiszemy w terminalu:

```
swift myProgram.swift Hello world
```

Po wykonaniu programu, otrzymamy następujący output:

```
Hello
world
```

## Głębszy Wgląd

Możliwość przekazywania argumentów do programu z wiersza poleceń jest szczególnie użyteczna, gdy chcemy zmienić działanie naszej aplikacji w zależności od parametrów. Dzięki temu możemy dostosowywać nasz program do różnych potrzeb i ułatwić jego używanie.

Na przykład, możemy sprawdzić, czy przekazane argumenty zawierają określone wartości i na tej podstawie wykonać różne akcje. W naszym przykładowym kodzie, zamiast wyświetlać wszystkie argumenty, możemy sprawdzić, czy pierwszym argumentem jest słowo "Hello" i w zależności od tego, wyświetlić różne powitania:

```Swift
import Foundation

let arguments = CommandLine.arguments
let firstArgument = arguments[1]

if firstArgument == "Hello" {
    print("Witaj!")
} else {
    print("Cześć!")
}
```

Dzięki temu, nasz program będzie bardziej interaktywny i elastyczny.

## Zobacz Również

Jeśli chcesz dowiedzieć się więcej o pracy z argumentami z wiersza poleceń w Swift, polecamy zapoznać się z dokumentacją języka oraz poniższymi linkami:

- [Dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Tutorial o obsłudze argumentów z wiersza poleceń w Swift](https://www.raywenderlich.com/1224214-command-line-programs-on-macos-tutorial-for-beginners)
- [Inne artykuły na temat Swift na naszym blogu](https://www.raywenderlich.com/archive/?s=swift)

Mamy nadzieję, że ten wpis był dla Ciebie pomocny. Pozostajemy do dyspozycji w razie pytań lub uwag w komentarzach poniżej!