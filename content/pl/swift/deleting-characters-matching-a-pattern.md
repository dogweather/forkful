---
title:    "Swift: Usuwanie znaków pasujących do wzorca"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w trakcie programowania musimy usuwać znaki o określonym wzorze. Jest to szczególnie przydatne, gdy pracujemy z ciągami znaków lub danymi wejściowymi, które mogą zawierać niepożądane lub błędne znaki. W tym artykule dowiesz się, jak w prosty sposób usunąć znaki pasujące do określonego wzorca w języku Swift.

## Jak to zrobić

Aby usunąć znaki pasujące do wzorca, musimy użyć funkcji `replacingOccurrences(of:with:)` wraz z wyrażeniem regularnym. Przeanalizujmy poniższy przykład:

```Swift
let text = "Usunę [ten] znak"
let removedBrackets = text.replacingOccurrences(of: "[", with: "").replacingOccurrences(of: "]", with: "")
print(removedBrackets)
```
Wyjście: Usunę ten znak

W powyższym przykładzie zastosowaliśmy funkcję `replacingOccurrences(of:with:)` dwa razy, aby usunąć dwie występujące nawiasy kwadratowe. Możemy także użyć wyrażenia regularnego, aby usunąć wszystkie znaki spoza określonego zakresu. Przykład:

```Swift
let text = "123ABC456XYZ789"
let removedNonDigits = text.replacingOccurrences(of: "[^0-9]", with: "", options: .regularExpression)
print(removedNonDigits)
```
Wyjście: 123456789 

W powyższym przykładzie użyliśmy wyrażenia regularnego `[^0-9]`, które oznacza "wszystkie znaki spoza zakresu od 0 do 9". Dzięki temu wszystkie litery zostały usunięte, a zostawione zostały tylko cyfry.

## Podgląd

Głębsze zrozumienie usuwania znaków pasujących do wzorca w języku Swift wymaga znajomości wyrażeń regularnych. Wyrażenia regularne to wyrażenia, które pozwalają na wyszukiwanie i manipulację ciągami znaków o określonym wzorze. W języku Swift wyrażenia regularne można używać w funkcji `replacingOccurrences(of:with:options:)`, a także w innych metodach dla typu `String` i `NSString`. Więcej informacji na temat wyrażeń regularnych i zastosowań w języku Swift znajdziesz w dokumentacji: [Wyrażenia regularne w Swift](https://developer.apple.com/library/archive/documentation/General/Conceptual/DevPedia-CocoaCore/RegularExpressions.html).

## Zobacz także

- [Wyrażenia regularne w Swift](https://developer.apple.com/library/archive/documentation/General/Conceptual/DevPedia-CocoaCore/RegularExpressions.html)
- [Podstawy wyrażeń regularnych](https://www.regular-expressions.info/tutorial.html)
- [NSRegularExpression - Dokumentacja Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)