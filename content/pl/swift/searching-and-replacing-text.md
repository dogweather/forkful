---
title:                "Swift: Wyszukiwanie i zastępowanie tekstu"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego
Wiele razy w trakcie programowania napotkasz sytuacje, w których konieczne będzie przeszukiwanie i zamiana tekstu. Może to wynikać z potrzeby zmiany określonego wyrażenia, poprawienia błędów lub po prostu dostosowania kodu do nowych wymagań. W takim przypadku szybkie i skuteczne narzędzia do wyszukiwania i zamiany tekstu mogą okazać się nieocenione.

## Jak to zrobić
Aby przeszukać i zmienić tekst w kodzie Swift, należy wykorzystać metodę `replacingOccurrences(of:with:)`. Przyjmuje ona dwa argumenty - pierwszy to wyszukiwane wyrazy, a drugi to wyraz lub ciąg znaków, który ma zastąpić znaleziony. Na przykład:
```Swift
let text = "Witamy wśród kodera"
let newText = text.replacingOccurrences(of: "Witamy", with: "Cześć")
print(newText) // Output: Cześć wśród kodera
```
Ponadto, istnieje możliwość wskazania dodatkowych parametrów, takich jak ignorowanie wielkości liter czy ustalenie limitu liczby zamian. Szczegóły i przykłady wykorzystania tej metody można znaleźć w oficjalnej dokumentacji języka Swift.

## Głębszy zanurzenie
Warto pamiętać, że przy użyciu metody `replacingOccurrences(of:with:)` tylko pierwsze wystąpienie danego wyrażenia będzie zamienione. Jeśli chcesz zamienić wszystkie wystąpienia, możesz wykorzystać metodę `replacingOccurrences(of:with:options:)`, która dodatkowo przyjmuje argument `options`. Możesz w nim określić, czy wymiana ma odbywać się dla wszystkich wystąpień, czy tylko dla pierwszego, a także czy ignorować wielkość liter.

## Zobacz również
- Oficjalna dokumentacja języka Swift: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID297
- Przykłady wykorzystania metody `replacingOccurrences(of:with:)`: https://www.hackingwithswift.com/example-code/strings/how-to-replace-a-substring-with-another-substring