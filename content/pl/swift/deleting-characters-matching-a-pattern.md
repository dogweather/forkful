---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Swift: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków pasujących do wzorca jest procesem, w którym programiści usuwają określone znaki z tekstu. Jest to przydatna umiejętność, ponieważ pozwala na szybkie i łatwe porządkowanie i przetwarzanie danych tekstowych. Programiści często wykonują ten krok w celu filtrowania danych lub przetwarzania tekstów.

## Jak to zrobić:

```Swift
let text = "ABC123DEF456GHI789"
let pattern = "[0-9]+"
let regex = try! NSRegularExpression(pattern: pattern, options: [])
let modifiedText = regex.stringByReplacingMatches(in: text, options: [], range: NSRange(text.startIndex..., in: text), withTemplate: "")
print(modifiedText)

// Output: ABCDEFGHI
```

## Duże zmiany:

Koncepcja usuwania znaków pasujących do wzorca ma swoje korzenie w programowaniu funkcyjnym, w którym funkcje są często wykorzystywane do przetwarzania danych. Alternatywą dla wykorzystania wyrażeń regularnych do usuwania znaków jest użycie metod wbudowanych w język Swift, takich jak `filter`. Implementacja tego procesu jest także umożliwiona przy użyciu innych bibliotek do wyrażeń regularnych, takich jak `RegularExpressionKit`.

## Zobacz także:

- Dokumentacja Swift o wyrażeniach regularnych: https://developer.apple.com/documentation/foundation/nsregularexpression#//apple_ref/doc/uid/TP40008757
- Przykładowe użycie wyrażeń regularnych w języku Swift: https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift