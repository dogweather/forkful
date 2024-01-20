---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie nowego projektu oznacza start od zera, definiowanie nowych celów, planów i funkcji. Programiści to robią, aby rozwiązać unikalne problemy, zrealizować innowacyjne pomysły lub nauczyć się nowych umiejętności.

## Jak to zrobić:

Rozpoczniemy od zainstalowania pakietu Cabal, który jest systemem budowy i pakietowania dla Haskell. Użyjemy go do utworzenia nowego projektu:

```Haskell
cabal update
cabal install cabal-install
```

Teraz jesteśmy gotowi do stworzenia nowego projektu. Tutaj nazwiemy go "HelloHaskell".

```Haskell
cabal init -n --is-executable -p HelloHaskell
```
Czytajmy tę komendę: `init` mówi Cabal, że chcemy utworzyć nowy projekt, `-n` oznacza, że chcemy projekt "minimalny", `--is-executable` oznacza, że chcemy program wykonywalny, a `-p HelloHaskell` jest nazwą naszego projektu.

W swoim nowym projekcie zobaczysz plik `HelloHaskell.hs`. Wklej w nim tę linię kodu, aby wydrukować "Hello, Haskell!".

```Haskell
main :: IO ()
main = putStrLn "Hello, Haskell!"
```

Uruchom program:

```Haskell
cabal run HelloHaskell
```

Powinieneś zobaczyć `Hello, Haskell!` na swoim ekranie.

## Deep Dive 

Kiedy piszesz w Haskellu, pracujesz na bazie wieloletniej historii rozwoju języków funkcyjnych. Haskellowi poprzednicy, tacy jak ML i Lisp, służyli jako fundamenty dla tego potężnego języka.

Inne alternatywne narzędzia do rozpoczęcia nowego projektu Haskell to Stack lub używanie GHC (Glasgow Haskell Compiler) bezpośrednio. Wybór narzędzia zależy od preferencji, potrzeb projektowych i złożoności.

Szczegóły implementacji, takie jak struktura projektu, testowanie jednostkowe, dokumentacja kodu, są określane przez preferencje programisty i wymagania projektu.

## Zobacz również

- Cabal User Guide: https://cabal.readthedocs.io/
- "Learn You a Haskell" (naucz się Haskell'a): http://learnyouahaskell.com/
- Oficjalna dokumentacja Haskell'a: https://www.haskell.org/documentation/