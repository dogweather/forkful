---
title:                "Pisanie do standardowego błędu"
aliases:
- /pl/haskell/writing-to-standard-error/
date:                  2024-02-03T19:33:20.828476-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie do standardowego błędu"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu (stderr) w Haskellu pozwala programom rozróżniać ich wyjście pomiędzy normalnymi wynikami a komunikatami o błędach. Jest to kluczowe dla sygnalizowania problemów i debugowania, bez zaśmiecania standardowego wyjścia (stdout), które często przenosi główne dane lub wynik programu.

## Jak to zrobić:
W Haskellu, pisanie do stderr jest proste dzięki modułowi `System.IO` z biblioteki podstawowej. Poniżej znajduje się podstawowy przykład demonstrujący:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "To jest komunikat o błędzie."
```

Wyjście tego programu do stderr będzie:

```
To jest komunikat o błędzie.
```

Jeśli pracujesz nad bardziej złożoną aplikacją, lub jeśli potrzebujesz lepszej kontroli nad logowaniem (w tym błędów), możesz optować za użyciem biblioteki stron trzecich. Jednym z popularnych wyborów jest `monad-logger`, który integruje się ze stylem programowania `mtl` w Haskellu. Oto mały fragment używający `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "To jest komunikat o błędzie używając monad-logger."
```

Kiedy uruchomiony, wersja `monad-logger` podobnie wypisuje komunikat o błędzie, ale jest wyposażona w więcej kontekstu, jak znaczniki czasu czy poziomy logowania, w zależności od konfiguracji:

```
[Error] To jest komunikat o błędzie używając monad-logger.
```

Obie metody służą celowi pisania do stderr, z wyborem w dużej mierze zależnym od złożoności i potrzeb twojej aplikacji.
