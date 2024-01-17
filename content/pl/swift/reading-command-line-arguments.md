---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Swift: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Odczytywanie argumentów z wiersza poleceń to proces, w którym programista pobiera informacje wprowadzone przez użytkownika podczas uruchamiania programu z linii poleceń. Robią to, aby dostosować działanie programu do indywidualnych potrzeb użytkownika. Na przykład, jeśli program odmierza czas, użytkownik może podać jako argument liczbę sekund, a jeśli oblicza odległość, użytkownik może podać długość w jednostce miary swojego wyboru.

## Jak to zrobić:

Możemy odczytać argumenty z wiersza poleceń w prosty sposób za pomocą obiektu argumentów w funkcji ```main```. Nie musimy importować żadnych bibliotek. Następnie możemy skorzystać z metody ```ArgumentParser``` , aby odczytać poszczególne argumenty i wyświetlić je użytkownikowi.

Przykładowa implementacja w języku Swift wyglądałaby następująco:

```Swift
func main(arguments: [String]) {
    let parser = ArgumentParser(arguments: arguments)
    let time = Int(parser.getStringArgument(argumentName: "czas"))
    let distance = Double(parser.getStringArgument(argumentName: "odległość"))

    if let time = time {
        print("Czas: \(time) seconds")
    }

    if let distance = distance {
        print("Dystans: \(distance) km")
    }
}

main(arguments: [czas: "10", odległość: "5.6"])
// Output: Czas: 10 seconds, Dystans: 5.6 km
```

## Wnikliwe spojrzenie:

Odczytywanie argumentów z wiersza poleceń jest procesem używanym już od lat w programowaniu. Wcześniej argumenty te były przekazywane do programów jako tekst, liczby lub symbole. Obecnie, dzięki rozwojowi języków programowania, możemy używać dedykowanych bibliotek do odczytywania argumentów z wiersza poleceń, co ułatwia i usprawnia naszą pracę.

Alternatywą dla odczytywania argumentów z wiersza poleceń jest tworzenie interfejsu użytkownika z wykorzystaniem GUI lub CLI (interfejsu wiersza poleceń). Jednak odczytywanie argumentów z wiersza poleceń jest szybszym i bardziej bezpośrednim sposobem dostarczania informacji do programu, szczególnie w przypadku prostych aplikacji.

## Zobacz też:

- [Dokumentacja ArgumentParser](https://developer.apple.com/documentation/swift/argumentparser)
- [Przykładowy projekt wykorzystujący odczytywanie argumentów z wiersza poleceń w języku Swift](https://github.com/johnkiddie/Swift-Command-Line-Argument-Example)