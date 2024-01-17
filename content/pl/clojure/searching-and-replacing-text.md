---
title:                "Wyszukiwanie i zastępowanie tekstu."
html_title:           "Clojure: Wyszukiwanie i zastępowanie tekstu."
simple_title:         "Wyszukiwanie i zastępowanie tekstu."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Czym jest wyszukiwanie i zastąpienie tekstu? Jest to proces, w którym programista szuka w określonej długości tekstu określonej frazy i zamienia ją na inną. Zazwyczaj jest to wykonywane, aby zmienić formatowanie lub uaktualnić zawartość tekstu.

Dlaczego programiści to robią? Istnieje wiele powodów, dla których programiści mogą chcieć przeprowadzić wyszukiwanie i zastąpienie tekstu. Może być to potrzebne do szybkiego zmieniania wielu wersów w kodzie, zmiany składni lub uaktualnienia danych w bazie danych.

### Jak to zrobić:
Przykłady kodu i wyników w zdawkowych blokach ```Clojure...```

Aby wyszukać i zastąpić tekst w Clojure, możesz użyć funkcji ```replace``` z biblioteki standardowej Clojure, która przyjmuje dwa argumenty: frazę do wyszukania i frazę, którą należy zastąpić. Na przykład:

```Clojure
(replace "hello" "hi" "hello world")
```
Wynik:
```Clojure
"hi world"
```

Możesz również użyć funkcji ```replace-first```, która zastąpi tylko pierwsze wystąpienie frazy, lub ```replace-regexp```, która pozwala na użycie wyrażeń regularnych.

### Głębokie zanurzenie:
Wyszukiwanie i zastępowanie tekstu jest popularną funkcją w wielu językach programowania, w tym w Clojure. Obecnie można także zastosować to narzędzie do wielu różnych typów dokumentów, na przykład w arkuszach kalkulacyjnych lub edytorach tekstu.

Alternatywą dla funkcji ```replace``` jest metoda ```str/replace``` z biblioteki ```clojure.string```, która działa na ciągach znaków i może być przydatna, jeśli konkretny tekst jest w zmiennej.

### Zobacz także:
- [Dokumentacja Clojure: Replace](https://clojuredocs.org/clojure.core/replace)
- [Funkcje narzędziowe Clojure do wyszukiwania i zastępowania tekstu](https://stackoverflow.com/questions/22423526/clojure-functions-for-find-and-replace-string-in-file)
- [Tutorial dla początkujących w Clojure](https://clojure.org/guides/getting_started)