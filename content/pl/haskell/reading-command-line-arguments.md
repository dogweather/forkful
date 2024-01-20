---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Czytanie argumentów z wiersza poleceń to proces, w którym program interpretuje dane wprowadzone przez użytkownika przy uruchomieniu. Programiści używają go, aby zwiększyć elastyczność i interaktywność swoich aplikacji.

## Jak to zrobić:

Aby odczytać argumenty z linii poleceć w Haskellu, użyjemy funkcji `getArgs` z modułu `System.Environment`. Oto prosty przykład:

```Haskell
import System.Environment  
  
main = do  
    args <- getArgs  
    putStrLn ("Cześć, " ++ args !! 0)
```

Uruchomiony z argumentem, np. `ghc app.hs Adam`, wyprodukuje wyjście:
```
Cześć, Adam
```

## Dogłębne zrozumienie:

Odczytywanie argumentów z linii poleceń to technika, która istnieje od początków informatyki. Kiedy używamy `getArgs`, Haskell wykorzystuje typy danych i model IO do bezpiecznego odczytania i manipulowania tymi argumentami. Inne języki, jak Python czy Ruby, mają podobne mechanizmy, ale rzadko są one tak eleganckie jak w Haskellu.

Alternatywnie, dla bardziej zaawansowanego i rozbudowanego parsowania argumentów z linii poleceń, można użyć biblioteki jak `optparse-applicative` lub `cmdargs`. Ta druga jest znacznie bardziej popularna, choć obie są godne uwagi. `cmdargs` pozwala definiować ramy i reguły dla oczekiwanych argumentów, co jest bardzo przydatne dla dużych projektów.

## Zobacz też:

- Moduł `System.Environment` [dokumentacja](http://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)
- Biblioteka `optparse-applicative` [dokumentacja](https://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html)
- Biblioteka `cmdargs` [dokumentacja](http://hackage.haskell.org/package/cmdargs-0.10.20/docs/System-Console-CmdArgs.html)