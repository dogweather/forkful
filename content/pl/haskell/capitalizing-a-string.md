---
title:                "Haskell: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# ## Dlaczego

Jeśli jesteś programistą Haskell, pewnie zastanawiasz się, dlaczego kiedykolwiek chciałbyś zmienić wielkość liter w ciągu znaków. Cóż, istnieje wiele powodów dla których mógłbyś tego potrzebować. Może chcesz podkreślić ważne słowa w swoim tekście, albo dostosować wyświetlanie informacji na ekranie. Niezależnie od przyczyny, nauka jak zmienić wielkość liter w Haskell jest wiedzą, którą warto posiąść.

 # ## Jak to zrobić

Aby zmienić wielkość liter w ciągu znaków, użyj funkcji `toUpper` lub `toLower`. Oba te funkcje przyjmują pojedynczy znak i zwracają jego odpowiednik w odpowiedniej wielkości liter. Przykładowo, aby zmienić wszystkie litery w ciągu na wielkie, możesz użyć funkcji `map` wraz z funkcją `toUpper`. Oto przykładowy kod:

```Haskell
import Data.Char

capitalize :: String -> String
capitalize str = map toUpper str
```

Wywołując funkcję `capitalize` na tekście "haskell", otrzymasz wynik "HASKELL". Podobnie, aby zmienić wszystkie litery na małe, możesz użyć funkcji `toLower`.

 # ## Głębsze wgląd

Jeśli chcesz poznać bardziej zaawansowane sposoby zmiany wielkości liter, możesz rozważyć użycie funkcji `capitalizeFirst` lub `capitalizeWords` z pakietu `text`. Funkcja `capitalizeFirst` zmienia tylko pierwszą literę w ciągu na wielką, natomiast `capitalizeWords` zmienia pierwszą literę każdego słowa. Innym sposobem jest użycie funkcji `toTitle` z pakietu `stringprep`, która zmienia na wielkie litery wszystkie wyrazy w ciągu, poza wyrazami znajdującymi się pomiędzy apostrofami.

 # ## Zobacz też

- [Dokumentacja funkcji toUpper i toLower](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#v:toUpper)
- [Funkcje z pakietu text do zmieniania wielkości liter](https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-CaseConversion.html)
- [Funkcja toTitle z pakietu stringprep](https://hackage.haskell.org/package/stringprep-1.1.1/docs/Data-StringPrep-Prop-W4-NamePrep-CaseFolding.html)