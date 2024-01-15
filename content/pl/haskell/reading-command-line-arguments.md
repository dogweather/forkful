---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Haskell: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli kiedykolwiek próbowałeś uruchomić program z wiersza poleceń, prawdopodobnie wpisywałeś różnego rodzaju opcje i argumenty. W tym artykule dowiesz się, jak w języku Haskell przetwarzać i wykorzystywać te argumenty.

## Jak to zrobić?

### Tworzenie prostej aplikacji

Aby zacząć, musimy zaimportować moduł `System.Environment` do naszego programu. Pomoże nam to w przetwarzaniu argumentów podanych przez użytkownika.

```
```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn (show args)
```

W tym prostym przykładzie, tworzymy nową funkcję o nazwie `main` i wykorzystujemy funkcję `getArgs` z modułu `System.Environment`. Następnie, wyświetlamy argumenty podane przez użytkownika przy użyciu funkcji `putStrLn`. Aby uruchomić ten program, wpisz w wierszu poleceń `runhaskell nazwa_pliku.hs argument1 argument2`.

### Przetwarzanie argumentów

W powyższym przykładzie, argumenty są przekazywane jako lista stringów, więc możemy łatwo wykorzystać funkcje dla list, takie jak `head` czy `tail`, aby uzyskać pojedyncze argumenty lub resztę argumentów. Na przykład, jeśli chcielibyśmy wyświetlić tylko drugi argument podany przez użytkownika:

```
```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn (args !! 1)
```

### Przetwarzanie flag

Czasami użytkownik może podać flagi wraz z argumentami, aby wskazać pewne ustawienia lub opcje. W takim przypadku, możemy skorzystać z funkcji `getOpt` z modułu `System.Console.GetOpt` aby przetworzyć te flagi. Przykładowy kod może wyglądać następująco:

```
```Haskell
import System.Environment
import System.Console.GetOpt

data Options = Options
  { optHelp :: Bool
  , optVerbose :: Bool
  } deriving Show

defaultOptions = Options
  { optHelp = False
  , optVerbose = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True })) "Show help"
  , Option ['v'] ["verbose"] (NoArg (\opts -> opts { optVerbose = True })) "Be verbose"
  ]

runWithOptions :: Options -> [String] -> IO ()
runWithOptions options args = do
  print options
  print args

main = do
  args <- getArgs
  let (options, leftoverArgs, errors) = getOpt Permute options args
  if null errors then
     runWithOptions (foldl (flip id) defaultOptions options) leftoverArgs
  else
     ioError (userError (concat errors))
```

W tym przykładzie, definiujemy własny typ danych `Options`, który zawiera flagi, które możliwe jest przekazanie przez użytkownika. Następnie, określamy domyślne wartości i flagi, które mogą być przetworzone przez naszą funkcję `runWithOptions`. Na koniec, przy użyciu funkcji `getOpt`, przetwarzamy flagi i przekazujemy ostateczne opcje oraz pozostałe argumenty do naszej funkcji `runWithOptions`.

## Głębsza analiza

Istnieje wiele innych metod przetwarzania i wykorzystania argumentów w języku Haskell, na przykład wykorzystując moduł `Data.Map` lub wykorzystując monady do wygodniejszej obsługi argumentów. Ważne jest, aby przetestować swoje rozwiązanie z różnymi przypadkami, aby upewnić się, że nasz program będzie działał poprawnie dla wszystkich user inputów.

## Zobacz także

* [Dokumentacja System.Environment](https://hackage.haskell.org/package/base-4.14.1.