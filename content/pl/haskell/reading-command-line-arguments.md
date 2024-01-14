---
title:    "Haskell: Odczytywanie argumentów wiersza poleceń"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Dlaczego

Często, podczas pisania programów, jesteśmy zmuszeni do podawania pewnych danych na samym początku wykonywania programu, np. ścieżki do plików, argumentów czy flag ustawień. Dzięki temu, nasze aplikacje mogą dostosować swoje działanie do obecnie panujących warunków lub priorytetów użytkownika. W tym artykule dowiesz się, jak w języku Haskell odczytać argumenty wiersza poleceń i wykorzystać je w swoim kodzie.

# Jak to zrobić?

Do obsługi argumentów wiersza poleceń w Haskellu wykorzystamy prosty moduł o nazwie System.Environment. Najpierw musimy zaimportować ten moduł do naszego kodu, a następnie skorzystać z funkcji getArgs, która zwróci listę z podanymi przez użytkownika argumentami. Poniżej znajduje się przykładowy kod:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn "Podane argumenty:"
    print args
```

Po uruchomieniu tego programu z argumentami wiersza poleceń:
```bash
ghc program.hs -o myprogram
```
Otrzymamy następujący wynik:
```
Podane argumenty:
["-o", "myprogram"]
```

Jeśli chcemy uzyskać dostęp do poszczególnych argumentów, możemy skorzystać z funkcji elemIndex, która zwraca pozycję danego elementu w liście. Przykładowo, jeśli chcemy odczytać drugi argument (w naszym przypadku "-o"), wykorzystamy tę funkcję w następujący sposób:

```Haskell
import System.Environment
import Data.List (elemIndex)

main :: IO ()
main = do
    args <- getArgs
    let index = elemIndex "-o" args
    case index of
        Just num ->
            putStrLn $ "Drugi argument: " ++ (args !! (num + 1))
        Nothing ->
            putStrLn "Nie znaleziono argumentu -o"
```
Wynik dla tego programu będzie wyglądać następująco:
```
Drugi argument: myprogram
```

# Głębszy zanurzenie

Powyżej przedstawione metody są wystarczające dla większości przypadków użycia argumentów wiersza poleceń. Jednak, jeśli chcemy odczytać bardziej skomplikowane argumeny lub skorzystać z flag ustawień, konieczne będzie wykorzystanie biblioteki optparse-applicative. Dzięki niej, możemy łatwo definiować nasze argumenty oraz ich wartości domyślne. Przykładowo, jeśli nasz program będzie posiadał opcję -v, która wypisze wersję programu, możemy to zdefiniować w ten sposób:

```Haskell
import System.Environment
import Options.Applicative

data ProgramOptions = ProgramOptions
    { verbose :: Bool }

parseOptions :: Parser ProgramOptions
parseOptions = ProgramOptions
    <$> switch (short 'v')

main :: IO ()
main = do
    options <- execParser (info (parseOptions <**> helper) fullDesc)
    if verbose options
        then putStrLn "Wersja programu: 1.0"
        else putStrLn "Wersja podstawowa"
```

# Zobacz także

- [System.Environment documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Options.Applicative documentation](https://hackage.haskell.org/package/optparse-applicative)
- [Haskell command-line argument parsing with optparse-applicative](https://www.fpcomplete.com/blog/2017/07/command-line-parser-optparse-applicative/)