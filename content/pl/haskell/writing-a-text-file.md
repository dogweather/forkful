---
title:                "Haskell: Pisanie pliku tekstowego"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego pisanie pliku tekstowego jest ważne

Pisanie plików tekstowych jest niezbędnym elementem programowania w Haskellu. Pozwala ono na zapisywanie danych w trwałej postaci, co jest niezwykle przydatne w różnego rodzaju aplikacjach. Poniżej przedstawione zostaną przykłady, jak można pisać pliki tekstowe w Haskellu oraz jak to zrobić w sposób bardziej zaawansowany.

## Jak to zrobić

Pierwszym krokiem jest importowanie modułu `System.IO`, który umożliwia operacje na plikach. Następnie, do stworzenia pliku tekstowego potrzebujemy funkcji `openFile` z dwoma argumentami: ścieżką do pliku oraz tryb otwarcia, który w tym przypadku będzie `WriteMode`:

```Haskell
import System.IO

main = do
    let fileName = "moj_plik.txt"
    handle <- openFile fileName WriteMode
```

Następnym krokiem będzie użycie funkcji `hPutStr` lub `hPutStrLn` do zapisu danych do pliku. Możemy zapisać pojedynczą linijkę tekstu lub cały napis:

```Haskell
hPutStr handle "To jest mój pierwszy plik tekstowy."
hPutStrLn handle "Kolejna linijka tekstu."
```

Ostatnim ważnym krokiem jest zamknięcie pliku za pomocą funkcji `hClose`:

```Haskell
hClose handle
```

Po wykonaniu tych kroków, plik tekstowy zostanie utworzony i zapisany w wybranej lokalizacji. Możemy także użyć funkcji `withFile`, która otwiera plik, wykonuje operacje na nim i automatycznie go zamyka:

```Haskell
import System.IO

main = do
    let fileName = "moj_plik.txt"
    withFile fileName WriteMode (\handle -> do
        hPutStr handle "To jest mój pierwszy plik tekstowy."
        hPutStrLn handle "Kolejna linijka tekstu."
    )
```

## Głębsza analiza

W przypadku gdy chcemy w trakcie działania programu dodać kolejne dane do pliku tekstowego, musimy użyć trybu `AppendMode`:

```Haskell
withFile fileName AppendMode (\handle -> do
    hPutStr handle "Nowe dane do pliku."
    hPutStrLn handle "Kolejna linijka tekstu."
)
```

Możemy także używać funkcji `hGetContents` oraz `hGetLine`, które pozwalają na odczytywanie danych z pliku tekstowego.

## Zobacz także

- [Dokumentacja modułu System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Tutorial w języku polskim o operacjach na plikach w Haskellu](https://www.ziemniak.ovh/?cat=oprogramowanie&subcat=informacje&art=2015-11-22-haskell-operacje-na-plikach)