---
title:                "Haskell: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu (stderr) jest nie tylko ważnym elementem programowania w Haskellu, ale także bardzo przydatnym narzędziem do debugowania. Poprzez wypisywanie błędów i informacji do stderr, programista może szybko zlokalizować i naprawić potencjalne problemy w swoim kodzie.

## Jak to zrobić

Aby pisać do standardowego błędu w Haskellu, wystarczy skorzystać z funkcji `hPutStrLn` z modułu `System.IO`. Przykładowy kod wykorzystujący tę funkcję wyglądałby następująco:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Błąd: nieznaleziono pliku"
```

Po uruchomieniu tego programu, na konsoli zostanie wypisany tekst "Błąd: nieznaleziono pliku" wraz z informacją o błędzie.

## Głębszy wgląd

Istnieją również inne funkcje z modułu `System.IO`, takie jak `hPutStr` i `hPutChar`, które pozwalają na pisanie do standardowego błędu bez dodawania nowej linii na końcu. Dodatkowo, istnieje również możliwość przekierowania standardowego błędu do pliku z pomocą funkcji `hPutStrLn`, co może być przydatne w przypadku zapisywania logów.

## Zobacz także

- [Dokumentacja modułu System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Poradnik dla początkujących w Haskellu](https://learnyouahaskell.com/)
- [Przydatne narzędzia do debugowania w Haskellu](https://medium.com/swlh/useful-tools-for-debugging-haskell-code-14d9779ae1d8)