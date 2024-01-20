---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Czytanie argumentów linii poleceń odnosi się do danych wejściowych, które są przekazywane do programu podczas jego uruchamienia. Programiści robią to, aby umożliwić swojemu oprogramowaniu, być bardziej elastycznym i interaktywnym.

## Jak to Zrobić?

Clojure (1.10.1) korzysta z obiektu `*command-line-args*` do przechowywania argumentów linii poleceć.
```Clojure
(let [args *command-line-args*]
  (println "Podane argumenty to: " args))
```

Kiedy uruchamiasz program z argumentami, np., `java -cp clojure.jar clojure.main my_script.clj arg1 arg2` dostajemy:
```
Podane argumenty to: (arg1 arg2)
```

## Pogłębione Zanurzenie

Czytanie argumentów linii poleceń ma długą historię, pochodzącą jeszcze od czasów, gdy interfejsy użytkownika były głównie oparte na tekście. Istnieją alternatywne metody przekazania danych do programu, takie jak strumienie wejściowe, pliki, interaktywne inputy użytkownika, itp.

W Clojure, `*command-line-args*` przechowuje argumenty jako listę znaków string, podobnie jak większość innych języków programowania. 

## Zobacz Również

1. Dokumentacja Clojure na temat globalnych zmiennych: [clojure.org](https://clojure.org/reference/vars#_dynamic)
2. Wstęp do programowania Clojure: [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
3. Obsługa argumentów linii poleceń w innych językach: [Python](https://docs.python.org/3/library/argparse.html#module-argparse), [Ruby](https://docs.ruby-lang.org/en/2.7.0/OptionParser.html)