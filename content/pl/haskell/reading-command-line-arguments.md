---
title:                "Haskell: Odczytywanie argumentów wiersza poleceń"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć czytelniku! Jeśli interesujesz się programowaniem w Haskellu, na pewno już wiesz, że jest to język funkcyjny, który jest bardzo silnie typowany. Cechuje się on wydajnością i bezpieczeństwem w działaniu, dlatego jest wykorzystywany w wielu projektach. W dzisiejszym poście chciałbym poruszyć temat odczytywania argumentów wiersza poleceń (command line arguments). Możesz się zastanawiać, dlaczego powinieneś/chciałbyś się na to szczególnie zwracać uwagę. W tym poście spróbuję Ci to wyjaśnić!

## Jak to zrobić

Odczytywanie argumentów wiersza poleceń jest w Haskellu bardzo proste, a dzięki temu możemy dostarczyć naszym programom dodatkową funkcjonalność. Aby odczytać argumenty, używamy funkcji `getArgs` z modułu `System.Environment`. Proponuję przejrzeć poniższy kod, aby zobaczyć jak to działa:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Podano argumenty: " ++ show args
```

Powyższy przykład wyświetli wszystkie przekazane argumenty wiersza poleceń po wykonaniu programu. Przykładowe wywołanie może wyglądać tak:

`$ program argument1 argument2`

Wówczas w konsoli wyświetlona zostanie wiadomość:

`Podano argumenty: ["argument1", "argument2"]`

Dzięki temu możemy łatwo przekazać dodatkowe informacje do naszego programu, co może być szczególnie przydatne przy pisaniu skryptów lub aplikacji z interfejsem wiersza poleceń.

## Głębszy zanurzania się w temat

Jednym z ciekawszych zastosowań odczytywania argumentów wiersza poleceń jest możliwość przekazania do naszego programu pliku konfiguracyjnego. W tym celu możemy wykorzystać bibliotekę `optparse-applicative`, która pozwala na łatwe parsowanie argumentów wiersza poleceń oraz tworzenia opcji programu.

Na przykład, jeśli nasz program wymaga pliku konfiguracyjnego, możemy utworzyć opcję `-c, --config` i odczytać ścieżkę do pliku jako argument. Wówczas w kodzie naszego programu wyglądałoby to podobnie do poniższego przykładu:

```Haskell
import Options.Applicative

data AppConfig = AppConfig
    { configFile :: FilePath
    -- inne pola konfiguracyjne
    }

configParser :: Parser AppConfig
configParser = AppConfig
    <$> strOption
        ( long "config"
        <> short "c"
        <> metavar "FILE"
        <> help "ścieżka do pliku konfiguracyjnego" )

main = do
    config <- execParser $ info (configParser <**> helper)
        ( fullDesc
        <> header "Nasz program"
        <> progDesc "Opis programu" )
    putStrLn $ "Odczytany plik konfiguracyjny: " ++ configFile config
```

Więcej informacji na temat biblioteki `optparse-applicative` możesz znaleźć w linkach poniżej.

## Zobacz również

- [Dokumentacja Haskell - System.Environment](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html)
- [Dokumentacja optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
- [Przykładowy projekt z wykorzystaniem optparse-applicative](https://github.com/pczajkowski/haskell-cli-example)