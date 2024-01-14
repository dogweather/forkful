---
title:    "Haskell: Odczytywanie pliku tekstowego"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie programów może być przyjemnym i satysfakcjonującym sposobem spędzania czasu. Jednak czasami musimy też zmierzyć się z mniej przyjemnymi zadaniami, takimi jak czytanie dużych plików tekstowych. Dlatego warto poznać narzędzia, które ułatwią nam ten proces.

## Jak to zrobić?

W języku Haskell mamy dostęp do wielu przydatnych funkcji i bibliotek, które mogą pomóc nam w czytaniu plików tekstowych. W poniższym przykładzie wykorzystamy funkcję `readFile` oraz moduł `Data.Text`, aby wczytać i wyświetlić zawartość pliku.

```Haskell
import qualified Data.Text as T

main = do
  fileContent <- readFile "nazwa_pliku.txt"
  let text = T.pack fileContent
  putStrLn text
```

Po uruchomieniu powyższego kodu, w konsoli zostanie wyświetlona zawartość pliku tekstowego o nazwie "nazwa_pliku.txt".

## Głębsze wertowanie

Istnieje wiele innych sposobów na czytanie plików tekstowych w Haskellu. Możemy użyć funkcji `hGetContents` zamiast `readFile`, aby uzyskać dostęp do zawartości pliku jako ciąg znaków, a nie obiektu `Text`. Możemy również skorzystać z funkcji `getLine` lub `getContents`, aby czytać plik wiersz po wierszu.

Ponadto, jeśli chcemy przetworzyć plik tekstowy z użyciem bardziej zaawansowanych narzędzi, możemy skorzystać z parserów tekstowych, takich jak Parsec czy Attoparsec.

## Zobacz też

- [Dokumentacja `Data.Text`](https://hackage.haskell.org/package/text)
- [Poradnik o czytaniu plików w Haskellu](http://learnyouahaskell.com/input-and-output#files-and-streams)