---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie tymczasowych plików to proces zapisywania danych do pliku, który ma być używany tylko przez krótki okres czasu. Programiści robią to, aby przechowywać dane, które są zmienne i tylko chwilowo potrzebne.

## Jak to zrobić:

Clojure udostępnia `clojure.java.io` namespace, który zawiera funkcję `make-temp`, aby utworzyć plik tymczasowy. Oto jak to zrobić:

```Clojure
(require '[clojure.java.io :as io])

(def temp-file (io/make-temp))

(println temp-file)
```

Wyjście będzie podobne do:

```Clojure
#object[java.io.File 0x6778f1a4 /tmp/clojure-1587111468591587111.tmp]
```

Plik tymczasowy jest teraz utworzony w domyślnej lokalizacji dla tymczasowych plików w systemie.

## Wgłębienie się

Tworzenie plików tymczasowych jest praktyką stosowaną od początków programowania. Pliki te są niezbędne, gdy dane nie muszą być trwale przechowywane lub gdy programista chce zminimalizować użycie pamięci.

Alternatywą dla tworzenia plików tymczasowych jest stosowanie struktur danych w pamięci, ale jest to zazwyczaj możliwe tylko dla mniejszych zestawów danych. Funkcja `io/make-temp` w Clojure tworzy plik tymczasowy w domyślnej lokalizacji systemu operacyjnego, ale można również określić ścieżkę dostępu i prefix.

Clojure jest programem uruchomieniowym dla Javy i korzysta z bibliotek Javy do tworzenia plików tymczasowych. Funkcja `io/make-temp` zwraca obiekt `java.io.File`, który reprezentuje plik tymczasowy.

## Zobacz też

Clojure - Praca z plikami: https://clojuredocs.org/clojure.java.io

Tworzenie plików tymczasowych w Javie: https://www.baeldung.com/java-create-temporary-file