---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wypisywanie na standardowy błąd (stderr) pozwala oddzielać normalny output programu od komunikatów o błędach. Programiści robią to, żeby łatwo przeanalizować i przekierować błędy bez mieszania ich z właściwym wynikiem działania programu.

## Jak zrobić:
```Haskell
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    hPutStrLn stderr "To jest komunikat błędu."
```
Po uruchomieniu, zobaczymy na standardowym błędzie:
```
To jest komunikat błędu.
```

## Zagłębiamy się
Standardowy błąd, czyli stderr, jest jednym z trzech głównych strumieni danych używanych w systemach POSIX, obok stdin i stdout. Historia jego powstania wiąże się z potrzebą efektywnego raportowania błędów. Alternatywą jest zapis do logu lub wykorzystanie bibliotek do logowania. W Haskellu, `hPutStrLn stderr` to niskopoziomowa funkcja, która bezpośrednio zapisuje ciąg znaków do stderr, pomijając buforowanie, które odbywa się w przypadku stdout.

## Zobacz także
- [Haskell Documentation for System.IO](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html)
- [Haskell Wiki on IO](https://wiki.haskell.org/IO_inside)