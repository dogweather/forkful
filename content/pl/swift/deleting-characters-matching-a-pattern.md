---
title:                "Swift: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego warto usuwać litery pasujące do wzoru

Usuwanie liter pasujących do wzoru może być niezbędne w różnych scenariuszach programowania. Na przykład, jeśli chcemy usunąć wszystkie spacje z tekstu lub wyeliminować znaki specjalne z danego ciągu znaków. Dzięki usuwaniu liter pasujących do wzoru możemy w prosty sposób uporządkować i przetworzyć dane zgodnie z naszymi potrzebami.

## Jak to zrobić

W Swift możemy użyć metody `replacingOccurrences` w celu usunięcia liter pasujących do wzoru. Służy ona do zamiany jednego lub więcej wystąpień danego ciągu znaków na inny ciąg znaków. Poniżej znajduje się przykładowy kod, który usuwa wszystkie cyfry z podanego tekstu i zwraca wynik w postaci napisu:

``` Swift
let text = "To jest przykładowy tekst 123"
let filteredText = text.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)
print(filteredText) // Output: "To jest przykładowy tekst "
```

W powyższym przykładzie wykorzystujemy wyrażenie regularne `[0-9]`, które pasuje do wszystkich cyfr od 0 do 9. Aby wykorzystać wyrażenia regularne w metodzie `replacingOccurrences`, musimy również ustawić opcję `options` na `.regularExpression`.

## Wnikliwa analiza

Wyrażenia regularne to bardzo przydatne narzędzie w programowaniu, pozwalające na dopasowywanie i manipulowanie danymi według określonych wzorców. Aby lepiej zrozumieć sposób działania usuwania liter pasujących do wzoru, warto przeczytać więcej na temat wyrażeń regularnych i ich zastosowań w Swift.

## Zobacz również

Jeśli chcesz poznać więcej ciekawych metod manipulacji tekstem w Swift, polecam zapoznanie się z poniższymi linkami:

- [Dokumentacja Swift o wyrażeniach regularnych](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Blog SwiftLee o usuwaniu znaków specjalnych](https://www.swiftdevcenter.com/regular-expression-everything-you-need-to-know/#Delete_Characters_Matching_the_Pattern)