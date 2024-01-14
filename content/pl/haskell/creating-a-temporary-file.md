---
title:    "Haskell: Tworzenie pliku tymczasowego."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest nieodłączną częścią programowania w Haskellu. Czy jesteś programistą, który chce nauczyć się tworzyć tymczasowe pliki w swoim kodzie? Ten artykuł przeznaczony jest specjalnie dla Ciebie!

## Jak to zrobić

Tworzenie tymczasowych plików w Haskellu jest bardzo proste i możesz to zrobić za pomocą kilku prostych funkcji. Najpierw musimy zaimportować moduł `System.IO`:

```Haskell
import System.IO
```

Następnie możemy użyć funkcji `withSystemTempFile`, która przyjmuje dwa argumenty: prefix (początkowa część nazwy tymczasowego pliku) i funkcję, która zostanie wywołana z dwoma argumentami - ścieżką pliku i uchwytem do jego manipulacji:

```Haskell
withSystemTempFile "plik_temp" $ \path handle -> do
  -- tutaj możemy wykonać operacje na pliku używając uchwytu do jego manipulacji
  -- na przykład, możemy zapisać do pliku tekstowy:
  hPutStrLn handle "To jest przykładowy tekst"
```

Po wykonaniu wszystkich operacji na pliku, zamknięcie i usunięcie go jest automatycznie wykonane przez funkcję `withSystemTempFile`. Warto również wspomnieć o funkcji `withTempDirectory`, która działa podobnie, ale tworzy tymczasowy katalog zamiast pliku.

## Głębsza analiza

Podczas wywoływania funkcji `withSystemTempFile` lub `withTempDirectory`, funkcja `getTemporaryDirectory` jest wywoływana w celu pobrania ścieżki do katalogu tymczasowego. W przypadku systemu Linux lub MacOS, jest to `/temp`, a w przypadku Windows, funkcja ta zwraca wartość zmiennej środowiskowej `TMPDIR` lub `TEMP`. Warto również wspomnieć, że te funkcje korzystają z modułu `hslogger` do zapisywania informacji o tworzonych plikach tymczasowych.

## Zobacz również

1. [Dokumentacja modułu System.IO](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
2. [Poradnik tworzenia i używania plików tymczasowych w Haskellu](https://nix-tips.github.io/tips/snippets/2015/03/11/temporary-file-directory-haskell.html)
3. [Przykładowy kod wykorzystujący funkcje do tworzenia plików tymczasowych](https://github.com/foulane/eskerek-arbolt/blob/master/lambda/Utilities/File.hs)