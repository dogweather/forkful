---
title:    "Javascript: Tworzenie pliku tymczasowego"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych może być bardzo ważnym elementem w programowaniu. Dzięki nim możemy tymczasowo przechowywać i przetwarzać dane, które są potrzebne tylko w określonym momencie.

## Jak to zrobić

Aby utworzyć tymczasowy plik w języku Javascript, musimy najpierw użyć odpowiedniej biblioteki. Jedną z najpopularniejszych jest Bluebird. Następnie, wykonujemy następujące kroki:

1. Tworzymy obiekt zawierający dane, które chcemy zapisać w pliku tymczasowym. Na przykład:

```Javascript
var data = {
    name: "John Doe",
    age: 30,
    occupation: "Programmer"
}
```

2. Wywołujemy funkcję `tmp.file()` z biblioteki Bluebird, przekazując jako argument obiekt zawierający nasze dane oraz opcje, takie jak rozszerzenie pliku czy prefiks nazwy. Na przykład:

```Javascript
tmp.file(data, {
    postfix: ".json",
    prefix: "user_"
}, function(err, path, fd) {
    // kod obsługujący ewentualne błędy
    // oraz wykonujący operacje na pliku tymczasowym
    console.log("Scieżka do pliku: " + path);
    console.log("Deskryptor pliku: " + fd);
});
```

3. W funkcji callback, która zostanie wykonana po utworzeniu pliku, możemy wykonywać różne operacje na naszych danych, np. zapisywać je do pliku lub odczytywać z niego.

Po zakończeniu pracy z plikiem tymczasowym, ważne jest jego usunięcie. W tym celu możemy wywołać funkcję `fs.unlink()`, przekazując jako argument ścieżkę do pliku lub jego deskryptor, który uzyskaliśmy w funkcji callback.

## Deep Dive

Tworzenie plików tymczasowych może być szczególnie przydatne w przypadku, gdy chcemy przetestować jakieś funkcjonalności, nie chcąc jednocześnie modyfikować danych w naszej aplikacji. Dzięki tymczasowym plikom, możemy zapisać dane, z którymi będziemy pracować w teście, a następnie po jego zakończeniu, usunąć je z naszej aplikacji.

Warto również pamiętać, że pliki tymczasowe są usuwane automatycznie podczas wyjścia z programu lub w przypadku wystąpienia błędu.

## Zobacz również

- [Dokumentacja biblioteki Bluebird](https://www.npmjs.com/package/bluebird)
- [Oficjalna dokumentacja Języka Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript)
- [Poradnik o używaniu plików tymczasowych w programowaniu](https://www.codingunit.com/codingunit-obfuscation-programming-tutorials)