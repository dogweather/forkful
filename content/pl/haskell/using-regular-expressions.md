---
title:    "Haskell: Używanie wyrażeń regularnych"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions to narzędzie, które pozwala nam na wygodne i szybkie przeszukiwanie oraz manipulację tekstem w programowaniu. Jeśli często pracujesz z plikami tekstowymi lub potrzebujesz dokonać pewnych zmian w wielu plikach jednocześnie, regular expressions mogą okazać się niezbędnym narzędziem w Twojej walce z kodem.

## Jak to zrobić

Aby rozpocząć korzystanie z regular expressions w Haskell, musimy zaimportować moduł "Text.Regex", do którego należy biblioteka "regex-posix". Następnie, możemy użyć funkcji "makeRegex", aby stworzyć obiekt typu "Regex" na podstawie wyrażenia regularnego. Przykładowo, aby znaleźć wszystkie liczby w tekście, możemy użyć wyrażenia "\d+", które oznacza dowolną cyfrę jedna lub więcej razy. Kod może wyglądać następująco:

```Haskell
import Text.Regex
import Text.Regex.Posix

main = do
    let regex = makeRegex "\\d+"
    let text = "Lorem ipsum dolor sit amet, 123 consectetuer adipiscing elit."
    let numbers = getAllTextMatches (text =~ regex :: [String])
    print numbers
```

Powinniśmy zobaczyć w konsoli wynik: ["123"]. W tym przykładzie, użyliśmy funkcji "getAllTextMatches", aby uzyskać wszystkie dopasowania wyrażenia regularnego w tekście. Istnieją również inne funkcje, takie jak "match" czy "subRegex", które pozwalają na bardziej precyzyjne przetwarzanie tekstu.

## Głębsza analiza

Istnieje wiele różnych elementów, które możemy wykorzystać w wyrażeniach regularnych. Na przykład, "\d" oznacza dowolną cyfrę, ale możemy także posługiwać się "\w" dla dowolnego znaku alfanumerycznego, "\s" dla białego znaku, "\b" dla granicy słowa, czy też znakami specjalnymi, takimi jak "\d" czy "\s". Oprócz tego, możemy używać operatorów "+" dla jeden lub więcej wystąpień, "?" dla zero lub jednego wystąpienia, "*" dla zero lub więcej wystąpień, czy też nawiasów dla grupowania wyrażeń. Aby poznać pełną listę możliwości, zalecamy zapoznanie się z dokumentacją biblioteki "regex-posix".

## Zobacz także

- Dokumentacja biblioteki "regex-posix": https://hackage.haskell.org/package/regex-posix
- Podstawy wyrażeń regularnych: https://regexone.com/
- Przewodnik po używaniu wyrażeń regularnych w Haskell: https://github.com/eccstartup/haskell-regex