---
title:    "Haskell: Tworzenie pliku tekstowego"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego pisać pliki tekstowe?

Pisanie plików tekstowych jest nieodłączną częścią wielu zadań programistycznych. Warto nauczyć się podstaw, aby później móc zastosować je w większych projektach lub pracy zawodowej.

## Jak to zrobić?

```Haskell
-- Tworzenie nowego pliku tekstowego
file <- openFile "plik.txt" WriteMode
-- Zapisanie tekstu do pliku
hPutStrLn file "Witaj, świecie!"
-- Zamykanie pliku
hClose file
```

Powyższy kod tworzy nowy plik tekstowy o nazwie "plik.txt" i zapisuje w nim tekst "Witaj, świecie!". Pamiętaj, że plik musi być zamknięty po zakończeniu zapisywania do niego danych.

## Głębszy zanurzenie

Pisanie plików tekstowych w Haskellu jest łatwe i intuicyjne. Można również użyć funkcji "withFile", która automatycznie zamyka plik po zakończeniu działania kodu, zapewniając wygodną obsługę.

``` Haskell
-- Tworzenie nowego pliku tekstowego i zapisanie danych do niego
withFile "plik.txt" WriteMode (\file -> do
  hPutStrLn file "Cześć, tu jestem!"
  hPutStrLn file "Mam nadzieję, że Twój dzień jest wspaniały!"
  -- Zamykanie pliku automatycznie
  )
```

Pamiętaj, że funkcja "withFile" wymaga podania dodatkowej akcji do wykonania na otwartym pliku w postaci funkcji anonimowej lub nazwanej.

## Zobacz również

- [Haskell Wiki](https://wiki.haskell.org/)
- [Haskell dla początkujących](https://stackoverflow.com/questions/1012573/getting-started-with-haskell)
- [Oficjalna strona języka Haskell](https://www.haskell.org/)