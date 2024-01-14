---
title:                "Swift: Usuwanie znaków pasujących do wzorca"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w procesie programowania konieczne jest usunięcie znaków z ciągu znaków (string), które pasują do określonego wzorca. Może to być potrzebne w celu uporządkowania i oczyszczenia danych lub aby dostosować ciągi znaków do określonych wymagań. W tym artykule omówimy jak można to zrobić w języku Swift i dlaczego może być to przydatne.

## Jak to zrobić

Aby usunąć znaki z ciągu znaków pasujące do wzorca, należy użyć metody `replacingOccurrences(of:with:)` wraz z wyrażeniem regularnym. Wyrażenia regularne to specjalne wzorce, które pozwalają na odnajdywanie i manipulowanie tekstem. W poniższym przykładzie usuniemy znaki z ciągu znaków, które nie są literami lub cyframi:

```Swift
let originalString = "A!pple2"
let cleanedString = originalString.replacingOccurrences(of: "[^a-zA-Z0-9]", with: "", options: .regularExpression)
print(cleanedString) // wyświetli "Apple2"
```

Można również zastosować wyrażenie regularne, aby pozostawić tylko określone znaki i usunąć wszystkie inne. Na przykład, poniższe wyrażenie usunie wszystkie znaki z ciągu znaków z wyjątkiem cyfr:

```Swift
let originalString = "A!pple2"
let cleanedString = originalString.replacingOccurrences(of: "[^0-9]", with: "", options: .regularExpression)
print(cleanedString) // wyświetli "2"
```

Możliwości jest wiele, a wyrażenia regularne pozwalają na bardzo precyzyjne manipulowanie tekstem. Warto poznać je bliżej, aby móc wykorzystać ich potencjał w swoim kodzie.

## Deep Dive

Aby lepiej zrozumieć jak działa usuwanie znaków pasujących do wzorca w języku Swift, warto przyjrzeć się trochę bliżej implementacji metody `replacingOccurrences(of:with:)`. Ta metoda jest częścią klasy `NSString`, która jest pośrednio używana w języku Swift, ponieważ typ `String` jest w rzeczywistości aliasem dla `NSString` w tym języku.

Metoda ta wykorzystuje wyrażenia regularne w systemie operacyjnym iOS lub macOS - silnik `NSRegularExpression` do odnalezienia i zastąpienia pewnych znaków w ciągu znaków. Wyrażenia regularne są wykorzystywane na niższym poziomie niż Swift, co oznacza, że są one bardzo wydajne i działają w podobny sposób na wszystkich platformach.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych i ich zastosowaniach w języku Swift, polecamy zapoznać się z poniższymi artykułami:

- [Dokumentacja Apple - Wyrażenia regularne](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Ray Wenderlich - Mastering Regular Expressions in Swift](https://www.raywenderlich.com/5765-mastering-regular-expressions-in-swift)
- [Hacking with Swift - Regular expressions in Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)