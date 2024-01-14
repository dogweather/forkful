---
title:    "Swift: Usuwanie znaków pasujących do wzorca"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Każdego dnia jako programiści spotykamy się z różnymi wyzwaniami i problemami, które musimy rozwiązać w naszym kodzie. Jednym z częstych problemów jest usuwanie znaków, które pasują do określonego wzoru. W tym artykule dowiesz się dlaczego może to być potrzebne i jak możesz sobie z tym poradzić w języku Swift.

## Jak to zrobić

Usuwanie znaków pasujących do wzoru może być niezbędne w sytuacjach, gdy chcemy uporządkować dane, usunąć zbędne informacje lub wyciągnąć określone elementy. W języku Swift możemy to zrobić na kilka sposobów, jednym z nich jest użycie metody `removeAll` na ciągu znaków oraz wyrażenia regularnego.

```Swift
let text = "Mój kod jest super123!"
let pattern = "[0-9]"
let newText = text.replacingOccurrences(of: pattern, with: "", options: .regularExpression, range: nil)
print(newText) // "Mój kod jest super!"
```

W powyższym przykładzie, używając wyrażenia regularnego `[0-9]`, usuwamy wszystkie cyfry z tekstu. Możemy również użyć innych wyrażeń regularnych, aby dopasować i usunąć inne znaki. Jest to przydatne szczególnie w sytuacjach, gdy chcemy pominąć pewne informacje lub je zastąpić innymi.

## Głębszy wgląd

Aby powyższy przykład był jasny i zrozumiały, warto wiedzieć kilka rzeczy na temat wyrażeń regularnych. Są to ciągi znaków, które pozwalają nam dopasowywać i manipulować tekstem w różny sposób. Możemy używać ich w języku Swift dzięki klasie `NSRegularExpression` oraz metodzie `replacingOccurrences`. Istnieje wiele różnych wyrażeń regularnych, więc warto poświęcić trochę czasu, aby zrozumieć ich działanie i jak je używać w naszym kodzie.

## Zobacz również

- [Podstawy wyrażeń regularnych w języku Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Przykładowe wyrażenia regularne do usuwania znaków](https://regexr.com/)
- [5 porad dotyczących wyrażeń regularnych w Swift](https://blog.bobthedeveloper.io/5-tips-for-writing-regular-expressions-in-swift-5e3cc0beecfd)