---
title:                "Swift: Użycie wyrażeń regularnych"
simple_title:         "Użycie wyrażeń regularnych"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać wyrażeń regularnych w programowaniu Swift?

Wyrażenia regularne są niezwykle potężnym narzędziem w programowaniu Swift. Pozwalają one na łatwą i skuteczną manipulację tekstowymi danymi. Dzięki nim możemy wyszukiwać, porównywać, zamieniać i walczyć z niechcianymi znakami w łańcuchach tekstowych. Jeśli pracujesz z dużą ilością tekstowych danych w swoim kodzie, wyrażenia regularne mogą znacznie ułatwić Ci życie. 

# Jak używać wyrażeń regularnych w Swift?

Używanie wyrażeń regularnych w Swift jest bardzo proste. Najpierw musisz zaimportować odpowiedni moduł używając polecenia ```import Foundation```. Następnie możesz tworzyć wyrażenia regularne za pomocą symbolu ```try```. Niektóre z podstawowych wyrażeń regularnych to:

- ```let emailRegex = try NSRegularExpression(pattern: "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$")```: wyrażenie regularne do sprawdzania poprawności adresu e-mail.
- ```let urlRegex = try NSRegularExpression(pattern: "^(https?:\\/\\/)?([\\w.-]+)\\.([a-z]{2,6}\\.?)(\\/[\\w\\s.-]*)*\\/?$")```: wyrażenie regularne do sprawdzania poprawności adresu URL.
- ```let passwordRegex = try NSRegularExpression(pattern: "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)[a-zA-Z\\d]{8,}$")```: wyrażenie regularne do sprawdzania siły hasła (minimum 8 znaków, co najmniej jedna mała litera, co najmniej jedna duża litera i co najmniej jedna cyfra).

Możesz również używać funkcji ```matches(in:options:range)``` aby znaleźć dopasowania do wyrażenia regularnego w danym tekście. Przykładowy kod może wyglądać następująco:

```Swift
if let match = emailRegex.firstMatch(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count)) {
  print("Wprowadzony adres e-mail jest poprawny.")
} else {
  print("Wprowadzony adres e-mail jest niepoprawny.")
}
```

# Głębsze zagłębienie w wyrażeniach regularnych w Swift

Podstawowe wyrażenia regularne mogą być bardzo przydatne, ale istnieje wiele innych funkcji i opcji, które warto poznać. Możesz na przykład użyć wyrażeń regularnych do wyodrębniania określonych części tekstu, zastępowania wyrażeń lub walidacji danych wejściowych. Możesz również używać symboli specjalnych, które pozwalają tworzyć bardziej skomplikowane wyrażenia. W Internecie znajdziesz wiele źródeł z przykładami wyrażeń regularnych w różnych językach programowania, w tym w Swift.

# Zobacz także

- [Dokumentacja Apple na temat wyrażeń regularnych w Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Blog Ray'a Wenderlicha: "Wyrażenia regularne w Swift" (ang.)](https://www.raywenderlich.com/262-regular-expressions-tutorial-for-swift-part-1)
- [Strona Regex101 - narzędzie do testowania wyrażeń regularnych w różnych językach, w tym w Swift](https://regex101.com/)