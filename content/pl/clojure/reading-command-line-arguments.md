---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Clojure: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co to jest i po co? 
Odczytywanie argumentów wiersza poleceń to proces pobierania danych podanych przez użytkownika podczas uruchamiania programu w terminalu. Programiści wykorzystują tę funkcję, aby umożliwić interakcję z użytkownikiem lub dostosować działanie swojego kodu w zależności od otrzymanych argumentów.

## Jak to zrobić: 
```Clojure
(def args (rest *command-line-args*))
(println "Podano argumenty:" args)
```
W tym przykładzie używamy funkcji ```rest``` w celu odrzucenia pierwszego argumentu, który zawsze jest nazwą pliku wykonywalnego. Następnie korzystamy z ```println``` do wyświetlenia otrzymanych argumentów.

#### Przykładowy output:
```
Podano argumenty: ["argument1" "argument2" "argument3"]
```

## Deep Dive:
Odczytywanie argumentów z wiersza poleceń jest popularną funkcją w wielu językach programowania, a także w samej Clojure. Wcześniej, np. w języku C, wymagało to ręcznego parsowania napisów, jednak dzięki funkcji ```*command-line-args*``` w Clojure jest to o wiele prostsze. Istnieją również alternatywne biblioteki, takie jak ```clj-argparse```, które oferują bardziej rozbudowane możliwości odczytywania argumentów.

## Zobacz też:
[Oficjalna dokumentacja Clojure dla *command-line-args*](https://clojuredocs.org/clojure.core/*command-line-args*)
[Biblioteka clj-argparse](https://github.com/flatland/clj-argparse)