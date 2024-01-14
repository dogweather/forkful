---
title:    "Haskell: Generowanie losowych liczb"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego
Generowanie losowych liczb jest powszechnym zadaniem w wielu programach i systemach komputerowych. Jest to przydatne w zadaniach takich jak symulacje, testowanie i tworzenie gier. W Haskellu istnieje wiele sposobów na generowanie losowych liczb, co czyni go idealnym językiem do tego celu.

## Jak to zrobić
Generowanie losowych liczb w Haskellu jest możliwe dzięki wbudowanej bibliotece `Random`. W celu użycia tej biblioteki, musimy ją zaimportować:
```Haskell
import System.Random
```
Następnie możemy użyć funkcji `randomR` do wygenerowania jednej losowej liczby z podanego zakresu, na przykład od 1 do 10:
```Haskell
randomR (1,10) :: IO Int
```
Możemy również wygenerować listę losowych liczb za pomocą funkcji `randomRs`. Na przykład, jeśli chcemy wygenerować listę pięciu losowych liczb z przedziału od 1 do 100, możemy to zrobić w ten sposób:
```Haskell
randomRs (1,100) 5 :: IO [Int]
```
Warto również wspomnieć, że funkcje z biblioteki `Random` korzystają z generatora pseudolosowego typu `StdGen`, dlatego musimy go najpierw zainicjalizować za pomocą funkcji `mkStdGen`. Na przykład:
```Haskell
let gen = mkStdGen 42
```
To pozwoli nam na generowanie tych samych losowych liczb za każdym razem, gdy użyjemy funkcji z biblioteki `Random` z tym samym generatorem.

## Głębszy zanurzenie
Podczas generowania losowych liczb w Haskellu należy pamiętać, że wyniki zawsze są deterministyczne, co oznacza, że przy użyciu tych samych parametrów i generatora, otrzymamy zawsze takie same wyniki. Co więcej, niektóre funkcje generujące losowe liczby w Haskellu mogą działać nieco inaczej, na przykład funkcja `random` generuje liczby za pomocą rozkładu normalnego, podczas gdy `randomR` używa równomiernego rozkładu.

Istnieją również inne sposoby generowania losowych liczb w Haskellu, takie jak użycie biblioteki `MonadRandom` lub wykorzystanie funkcji `randomIO` z modułu `System.Random`. Możesz eksperymentować z różnymi sposobami i znaleźć ten, który najlepiej pasuje do Twojego projektu.

## Zobacz również
- [Dokumentacja biblioteki Random w Haskellu](https://hackage.haskell.org/package/random)
- [Blog Haskell School of Music - Losowość](https://haskellformusicians.com/2013/08/randomness/) 
- [Oficjalna strona języka Haskell](https://www.haskell.org/)