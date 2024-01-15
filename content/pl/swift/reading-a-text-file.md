---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Swift: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedyś potrzebować dostępu do danych przechowywanych w pliku tekstowym w języku Swift? Ten artykuł jest dla Ciebie! Dowiesz się, jak szybko i łatwo odczytać plik tekstowy w swojej aplikacji, używając tylko kilku linijek kodu.

## Jak to zrobić

Aby odczytać plik tekstowy w języku Swift, musimy najpierw utworzyć ścieżkę do tego pliku, a następnie użyć klasy `String` do odczytania zawartości. Załóżmy, że nasz plik tekstowy nazywa się "dane.txt" i znajduje się w tym samym katalogu co nasza aplikacja. Najpierw utwórzmy ścieżkę do tego pliku:

```Swift
let path = Bundle.main.path(forResource: "dane", ofType: "txt")
```

Następnie użyjmy metody `String(contentsOfFile:encoding:)` do odczytania zawartości pliku i przypisania jej do zmiennej `content`:

```Swift
let content = try! String(contentsOfFile: path!, encoding: .utf8)
```

Teraz możemy wyświetlić zawartość pliku za pomocą funkcji `print()`:

```Swift
print(content)
```

Jeśli nasz plik tekstowy zawierałby np. następujące dane:

```
Imię: Anna
Wiek: 25
Hobby: programowanie
```

To wyświetlenie zawartości pliku przez naszą aplikację wyglądałoby tak:

```
Imię: Anna
Wiek: 25
Hobby: programowanie
```

## Deep Dive

Metoda `String(contentsOfFile:encoding:)` umożliwia nam odczytanie zawartości pliku tekstowego w języku Swift w określonym kodowaniu, w tym przypadku użyliśmy kodowania UTF-8. Jeśli jednak nasz plik tekstowy jest kodowany inaczej, np. w ASCII, będziemy musieli dostosować kodowanie w naszym kodzie.

Ponadto, można również użyć metody `String(contentsOfFile:usedEncoding:)`, która pozwala nam odczytać kodowanie pliku bez konieczności wybierania go ręcznie.

Warto również zauważyć, że metoda `String(contentsOfFile:encoding:)` może generować błędy, gdy np. plik nie istnieje lub nie można go odczytać z powodu problemów z uprawnieniami dostępu. Dlatego ważne jest, aby uwzględnić obsługę błędów w naszym kodzie.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o operacjach na plikach w języku Swift, możesz zapoznać się z następującymi artykułami:

- [Writing Files in Swift](https://medium.com/ios-seminar/writing-files-in-swift-6dd4e39f889a)
- [Reading and Writing Files in Swift (Hacking with Swift)](https://www.hackingwithswift.com/articles/118/reading-and-writing-files-in-swift)
- [Manipulating Files in Swift with FileManager (NSHipster)](https://nshipster.com/nspathutilities/)