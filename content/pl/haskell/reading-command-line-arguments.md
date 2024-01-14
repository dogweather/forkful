---
title:    "Haskell: Odczytywanie argumentów wiersza poleceń"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedykolwiek napisać program, który musiał odczytać argumenty wprowadzone przez użytkownika z wiersza poleceń? Jeśli tak, lub jeśli jesteś ciekaw jak to zrobić przy użyciu Haskella, ten artykuł jest dla Ciebie!

## Jak to zrobić

Aby odczytać argumenty wprowadzone przez użytkownika w wierszu poleceń, musimy wykorzystać bibliotekę System.Environment, która zawiera funkcje do obsługi argumentów w Haskellu. Spójrzmy na prosty przykład:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Podałeś " ++ show (length args) ++ " argumentów."

```

Jeśli skompilujemy i uruchomimy ten program, a następnie wprowadzimy kilka argumentów po jego nazwie, otrzymamy następujący wynik:

```
$ ./program arg1 arg2 arg3 arg4
Podałeś 4 argumentów. 
```

Jak widzimy, argumenty wprowadzone przez użytkownika są przechowywane w liście, którą następnie możemy przetwarzać w dowolny sposób.

## Deep Dive

W przypadku bardziej zaawansowanych przypadków, gdzie chcemy np. określić opcje lub flagi, które program może przyjąć, warto zajrzeć do biblioteki System.Console.GetOpt. Pozwala ona na wygodną obsługę argumentów w stylu linii poleceń w innych językach programowania. Przykład użycia tej biblioteki może wyglądać następująco:

```Haskell
import System.Environment
import System.Console.GetOpt
import System.IO
import Data.Maybe

-- Definiujemy nasze opcje/flagi jako konstruktory typu Optycja
data Opcja = Wyswietl | Pomoc

-- Definiujemy mapowanie dla naszych opcji/flag
opcje :: [OptDescr Opcja]
opcje =
  [ Option ['w'] ["wyswietl"] (NoArg Wyswietl) "Wyswietla zadane dane.",
    Option ['h','?'] ["pomoc"] (NoArg Pomoc) "Wyświetla pomoc."
  ]

-- Funkcja pomocnicza, która odpowiada za przetwarzanie argumentów
procesujOpcje :: [String] -> IO ([Opcja], [String])
procesujOpcje argv =
  case getOpt Permute opcje argv of
    (o, n, []) -> return (o, n)
    (_, _, err) -> fail (concat err ++ usageInfo naglowek opcje)

-- Funkcja pomocnicza, która odpowiada za wyświetlenie nagłówka informującego o użyciu flag
naglowek :: String
naglowek = "Usage: program [-w | -h] input_file"

-- Główna funkcja programu
main :: IO ()
main = do
  -- Sprawdzamy, czy podane zostały jakieś argumenty
  args <- getArgs
  if null args
    then putStrLn "Nie podano argumentów!"
    else do
      -- Przetwarzamy argumenty
      (opcje, plik) <- procesujOpcje args
      -- Sprawdzamy, czy podano flagę "pomoc"
      when (elem Pomoc opcje) $
        putStrLn $ "To jest pomoc dla tego programu."
      -- Sprawdzamy, czy podano flagę "wyswietl" i czy podano nazwę pliku
      when (elem Wyswietl opcje && not (null plik)) $
        readFile (head plik) >>= putStrLn

```

Przykładowe wywołanie programu może wyglądać następująco:

```
$ ./program -w input.txt
Witaj w programie! W czytanie argumentów jesteśmy już mistrzami!
```

## Zobacz także

- [Dokumentacja System.Environment](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- [Dok