---
title:    "Swift: Używając wyrażeń regularnych"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego codziennego życia. Jednym z elementów, który zyskał ogromną popularność w świecie programistycznym, jest wyrażenia regularne (ang. regular expressions). Dlaczego warto je stosować i jak mogą ułatwić tworzenie programów?

## Jak To Zrobić

Wyrażenia regularne są bardzo przydatnym narzędziem, które pozwalają na szybkie i precyzyjne przeszukiwanie tekstu w poszukiwaniu określonych wzorców. W języku Swift, aby wykorzystać wyrażenia regularne, musimy najpierw zaimportować bibliotekę `Foundation`, która zawiera klasy i funkcje do obsługi wyrażeń regularnych.

Przykładowo, jeśli chcemy znaleźć wszystkie numery telefonów w danym tekście, możemy to zrobić za pomocą następującej funkcji:

```Swift
let phoneNumberRegex = try! NSRegularExpression(pattern: "[0-9]{3}-[0-9]{3}-[0-9]{4}")
let text = "Twój numer telefonu to: 123-456-7890"
let results = phoneNumberRegex.matches(in: text, range: NSRange(text.startIndex..., in: text))

results.forEach { result in
    print(text[Range(result.range, in: text)!])
}
```

Kod ten wykorzystuje wyrażenie regularne `[0-9]{3}-[0-9]{3}-[0-9]{4}` do znalezienia wszystkich numerów telefonów w tekście. Następnie, za pomocą pętli `forEach`, wyświetlamy wszystkie pasujące wyniki.

## W Głąb

Wyrażenia regularne w języku Swift oferują wiele możliwości, które pozwalają na dokładne dopasowanie i manipulację tekstem. Na przykład, możemy użyć operatora `~=` do sprawdzenia, czy dany tekst pasuje do wyrażenia regularnego, lub użyć funkcji `replacingOccurrences(of:with:)` do zastąpienia dopasowanych fragmentów tekstu.

Ponadto, wyrażenia regularne w Swift obsługują różne opcje, takie jak ignorowanie wielkości liter czy dopasowywanie tylko do całych wyrazów. Aby dowiedzieć się więcej o możliwościach wyrażeń regularnych w języku Swift, warto zapoznać się z oficjalną dokumentacją.

## Zobacz również

- [Dokumentacja języka Swift o wyrażeniach regularnych](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Przykładowe zastosowania wyrażeń regularnych w języku Swift](https://medium.com/swift2go/regular-expressions-in-swift-3-dd7e1acbf142)
- [Poradnik wideo o wyrażeniach regularnych w języku Swift](https://www.youtube.com/watch?v=t19SoHxCc_o)