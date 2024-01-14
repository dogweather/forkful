---
title:                "Swift: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Przy tej okazji zajmiemy się tematem łączenia ciągów znaków w języku Swift. Często spotykamy się z sytuacją, w której musimy połączyć kilka fragmentów tekstu w jedną całość. Na przykład, może być to złożenie imienia i nazwiska w bazie danych lub dodanie liczby do nazwy pliku. Dlatego warto poznać ten prosty, ale niezwykle przydatny mechanizm programowania.

## Jak to zrobić

Do łączenia ciągów znaków w języku Swift wykorzystujemy operator "+" lub metodę "append". Operator "+" jest używany do połączenia dwóch lub więcej ciągów znaków, natomiast metoda "append" dodaje nowy ciąg znaków do istniejącego. Przykładowe użycie wygląda następująco:

```Swift
let firstName = "Jan"
let lastName = "Kowalski"

let fullName = firstName + " " + lastName
print(fullName) // wynik: Jan Kowalski

var file = "image"
file.append("-01.png")
print(file) // wynik: image-01.png
```

W powyższych przykładach użyliśmy również specjalnego znaku spacji (" ") do oddzielenia imienia i nazwiska oraz łączenia z nazwą pliku. Warto zauważyć, że operacja łączenia ciągów działa również z innymi typami, takimi jak liczby czy nawet tablice.

## Wnikliwe spojrzenie

Podczas łączenia ciągów znaków w języku Swift warto pamiętać o kilku ważnych rzeczach. Po pierwsze, warto korzystać z metody "append", gdyż jest ona bardziej wydajna niż operator "+". Po drugie, można również wykorzystać operator "+=" do dodawania ciągów do istniejącego ciągu znaków. Po trzecie, jeśli chcemy połączyć większą ilość ciągów, warto skorzystać z metody "joined" lub "joined(separator:)", która umożliwia ustawienie właściwego znaku lub ciągu znaków pomiędzy poszczególnymi elementami.

## Zobacz również

Aby dowiedzieć się więcej o łączeniu ciągów znaków w języku Swift, warto przeczytać następujące artykuły (w języku angielskim):

- [Concatenating Strings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-concatenate-strings-and-variables-into-a-string)
- [String Interpolation in Swift](https://www.swiftbysundell.com/basics/string-interpolation/)
- [5 Tips for Cleaning Up String Concatenation in Swift](https://medium.com/@infinitecortex/5-tips-for-cleaning-up-string-concatenation-in-swift-6af228e9b69a)