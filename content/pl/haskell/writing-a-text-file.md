---
title:    "Haskell: Tworzenie pliku tekstowego"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią programowania w Haskell. Dzięki temu, możemy przechowywać dane w czytelnej formie i wykorzystywać je w naszych aplikacjach. Pisanie plików tekstowych jest również ważne w celu komunikacji z innymi programami.

## Jak to zrobić

Używając funkcji `writeFile`, możemy łatwo zapisać dane do pliku tekstowego. Przykładem może być zapisanie listy liczb do pliku tekstowego "numery.txt":

```Haskell
let numery = [1,2,3,4,5]
writeFile "numery.txt" $ unlines $ map show numery
```

Powyższy kod najpierw tworzy listę liczb, a następnie przy pomocy funkcji `writeFile` zapisuje je do pliku "numery.txt". Funkcja `unlines` oznacza, że każdy element listy zostanie zapisany w osobnym wierszu, a funkcja `map` przekształci każdą liczbę na ciąg znaków przy pomocy funkcji `show`.

Po uruchomieniu tego kodu, w pliku "numery.txt" pojawi się następująca zawartość:

```
1
2
3
4
5
```

## Głębszy zanurzenie

Wykorzystując funkcję `writeFile`, możemy także pisać bardziej skomplikowane pliki tekstowe, np. z wykorzystaniem CSV lub JSON. W przypadku CSV, możemy użyć funkcji `intersperse` z pakietu `Data.List`, aby dodać przecinki między elementami listy:

```Haskell
import Data.List (intersperse)

let nazwiska = ["Kowalski", "Nowak", "Jankowski"]
writeFile "nazwiska.csv" $ intercalate "," $ map show przecinki
```

Po uruchomieniu tego kodu, plik "nazwiska.csv" będzie miał następującą zawartość:

```
Kowalski,Nowak,Jankowski
```

W przypadku formatu JSON, możemy użyć funkcji `encode` z pakietu `Data.Aeson` do przekształcenia danych na postać JSON:

```Haskell
import Data.Aeson (encode)

let osoba = [("imie","Jan"),("nazwisko","Kowalski"),("wiek",45)]
writeFile "osoba.json" $ encode osoba
```

Po uruchomieniu tego kodu, plik "osoba.json" będzie zawierał następującą zawartość:

```
[{"imie":"Jan","nazwisko":"Kowalski","wiek":45}]
```

## Zobacz również

- [Funkcja `writeFile` w dokumentacji Hackage](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:writeFile)
- [Pakiet Data.List w dokumentacji Hackage](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
- [Pakiet Data.Aeson w dokumentacji Hackage](https://hackage.haskell.org/package/aeson)