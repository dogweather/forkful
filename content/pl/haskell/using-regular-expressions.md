---
title:                "Haskell: Użycie wyrażeń regularnych"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions są niezwykle przydatne w programowaniu, ponieważ pozwalają nam na wyrażanie reguł wyszukiwania i manipulowania tekstem w sposób bardziej złożony niż prosty ciąg znaków. Są to doskonałe narzędzie dla programistów, którzy chcą mieć większą kontrolę nad tym, jak ich programy przetwarzają i analizują dane tekstowe.

## Jak to zrobić

Aby użyć regular expressions w Haskellu, najpierw musimy zaimportować moduł "Text.Regex". Następnie możemy wykorzystać funkcje takie jak `matchRegex` lub `subRegex` do odpowiadających im operacji. Na przykład:

```Haskell
import Text.Regex

main = do
  let text = "Haskell jest najlepszym językiem programowania!"
  let pattern = "programowania"
  putStrLn $ "Tekst oryginalny: " ++ text
  putStrLn $ "Zastosowany wzorzec: " ++ pattern
  let matches = matchRegex (mkRegex pattern) text
  putStrLn $ "Znalezione dopasowania: " ++ show matches
  let replaced = subRegex (mkRegex pattern) text "tworzenia oprogramowania"
  putStrLn $ "Tekst po zastąpieniu: " ++ replaced
```

Przykładowy wynik:

```
Tekst oryginalny: Haskell jest najlepszym językiem programowania!
Zastosowany wzorzec: programowania
Znalezione dopasowania: Just "programowania"
Tekst po zastąpieniu: Haskell jest najlepszym językiem tworzenia oprogramowania!
```

## Głębsza analiza

Regular expressions w Haskellu są wykorzystywane przez moduł "Text.Regex" z pakietu "regex-base". Jest to interfejs ogólny, który udostępnia kilka funkcji, takich jak `RegexMaker` i `RegexLike`, do tworzenia i manipulowania wyrażeń regularnych. Więcej informacji na temat tych funkcji oraz innych modułów wykorzystujących regular expressions można znaleźć na stronie Hackage [regex](https://hackage.haskell.org/package/regex) i [regex-base](https://hackage.haskell.org/package/regex-base).

Ponadto, warto pamiętać o sposobie, w jaki Haskell obsługuje wyrażenia regularne. Na przykład, znak "\" jest specjalnym znakiem w Haskellu, więc musimy użyć podwójnego znaku "\\" aby oznaczyć ten znak w wyrażeniu regularnym. Jest to tylko jeden z przykładów różnic między wzorcami regularnymi w innych językach programowania a w Haskellu.

## Zobacz także

- [Haskell Regex Mini-Tutorial](https://wiki.haskell.org/Regular_expressions)
- [Haskell Regex Tutorial](https://www.tutorialspoint.com/haskell/haskell_regular_expressions.htm)
- [Regular Expressions on Hackage](https://hackage.haskell.org/packages/search?terms=regex)