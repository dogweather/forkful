---
title:                "Odczytywanie argumentów z linii poleceń"
html_title:           "Swift: Odczytywanie argumentów z linii poleceń"
simple_title:         "Odczytywanie argumentów z linii poleceń"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedyś korzystać z programów działających w wierszu poleceń? W takim przypadku na pewno spotkałeś się z pojęciem "command line arguments" - czyli argumentów przekazywanych do programu podczas uruchamiania. W tym artykule dowiecie się, dlaczego warto zapoznać się z tym tematem i jak to zrobić w języku Swift.

## Jak wykorzystać command line arguments w Swift

Aby poznać argumenty wywołania programu, wystarczy skorzystać z obiektu `CommandLine` oraz jego właściwości `arguments`. W poniższym przykładzie wywołanie programu `myProgram` z argumentami `firstArg` i `secondArg` wyświetli je w konsoli.

```Swift
let arguments = CommandLine.arguments
print("Arguments: \(arguments)")
```

Output:
`Arguments: ["myProgram", "firstArg", "secondArg"]`

Możemy również wykorzystać opcjonalną wartość argumentów, aby przekazać do programu dodatkowe informacje. W poniższym przykładzie, jeśli do programu zostanie podany argument `name`, zostanie wyświetlone powitanie z podanym imieniem. W przeciwnym razie, program wyświetli standardowe powitanie.

```Swift
let arguments = CommandLine.arguments
if let name = arguments.first {
    print("Hello, \(name)!")
} else {
    print("Hello, world!")
}
```

Output przy wywołaniu z argumentem `John`:
`Hello, John!`

Output bez argumentu:
`Hello, world!`

## Przemierzając argumenty głębiej

Poza prostym wykorzystaniem argumentów podczas wywołania programu, warto również poznać niektóre właściwości obiektu `CommandLine` oraz sposoby manipulacji argumentami. Na stronie [dokumentacji](https://developer.apple.com/documentation/foundation/commandline) możesz znaleźć więcej informacji na ten temat.

Warto także zauważyć, że argumenty są traktowane jako `String`, więc jeśli chcemy wykorzystać je w inny sposób, musimy przeprowadzić odpowiednie konwersje. Możemy również przekazywać do programu tablice argumentów, dzieląc je na mniejsze części, lub stosować zagnieżdżone struktury argumentów.

## Zobacz również

- [Dokumentacja języka Swift](https://swift.org/documentation/)
- [Kurs online dla początkujących w języku Swift](https://www.udemy.com/course/ios-13-app-programming-for-beginners/?referralCode=9BBB3CBBF0281884ABA0)
- [Inne artykuły o Swift na naszym blogu](https://www.example.com/blog/tag/swift/)