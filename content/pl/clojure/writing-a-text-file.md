---
title:                "Zapisywanie pliku tekstowego"
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
"## Co i dlaczego?"

Zapisywanie pliku tekstowego to proces zapisania danych w formie czytelnej dla człowieka do pliku na dysku. Programiści robią to, aby trwale zapisywać informacje, takie jak konfiguracje, logi czy wyniki pracy programu.

## How to:
"## Jak to zrobić:"

Do zapisu pliku tekstowego w Clojure używamy funkcji `spit`. Oto przykład:

```clojure
(spit "przyklad.txt" "Witaj, Clojure!")
```

Jeśli otworzysz `przyklad.txt`, zobaczysz:

```
Witaj, Clojure!
```

Do dopisywania treści użyj opcji `:append true`:

```clojure
(spit "przyklad.txt" "\nDopiszmy coś więcej." :append true)
```

Teraz `przyklad.txt` wygląda tak:

```
Witaj, Clojure!
Dopiszmy coś więcej.
```

## Deep Dive
"## Szczegółowe informacje"

`spit` to nowoczesny sposób na zapis plików w Clojure, ale korzeni szukaj w starszej funkcji `with-open` i Java `FileWriter`. Alternatywą jest bezpośrednie użycie Javy przez interop. W implementacji `spit` obiekty są konwertowane na łańcuchy znaków i zapisywane, co pokazuje elastyczność Clojure w integracji z Javą i łatwość obsługi danych w różnych formatach.

## See Also
"## Zobacz także"

- Oficjalna dokumentacja Clojure: https://clojure.org/
- Przewodnik po `spit` i `slurp`: https://clojuredocs.org/clojure.core/spit
- Clojure for the Brave and True - Rozdział o obsłudze plików: https://www.braveclojure.com/IO/