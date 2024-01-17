---
title:                "Pisanie pliku tekstowego"
html_title:           "Haskell: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Pisanie pliku tekstowego to proces zapisywania danych na dysku twardym komputera w postaci tekstu. Programiści często wykorzystują to narzędzie, aby zapisywać wyniki działania swoich programów lub przechowywać ważne informacje.

## Jak to zrobić:

```Haskell
import System.IO

main = do 
  writeFile "plik.txt" "To jest tekst, który zostanie zapisany do pliku."
```

W powyższym przykładzie wykorzystaliśmy funkcję `writeFile` z modułu `System.IO`, aby zapisać tekst do pliku o nazwie "plik.txt". Możemy także wczytać już istniejący plik używając funkcji `readFile`.

## Wgląd w temat:

Pisanie plików tekstowych jest jednym z podstawowych narzędzi, które programiści wykorzystują w swojej pracy. Od czasów mainstreamowego rozwoju informatyki, pisanie plików tekstowych było jedynym sposobem na zapisywanie danych na dysku. Obecnie istnieją również inne sposoby, takie jak bazy danych czy chmury, ale pisanie plików tekstowych nadal jest niezwykle ważnym elementem programowania.

## Zobacz też:

Dokumentacja modułu [System.IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html) z funkcjami do operacji na plikach.

Więcej o historii rozwoju programowania i wykorzystywaniu plików tekstowych można przeczytać w artykule na [Wikipedii](https://pl.wikipedia.org/wiki/Plik_tekstowy).