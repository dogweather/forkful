---
title:    "Clojure: Odczytywanie argumentów linii poleceń"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się korzystać z wiersza poleceń w swoich programach w Clojure? Jeśli tak, to wiesz, jakie to może być przydatne narzędzie. Ale czy wiesz, dlaczego warto nauczyć się czytać argumenty z wiersza poleceń? Ten artykuł odpowie na to pytanie.

## Jak to zrobić

Oto prosty przykład kodu w Clojure, który czyta argumenty z wiersza poleceń i wypisuje je na ekran:

```Clojure
(def args (rest *command-line-args*)
(dorun (map println args)))
```

Dzięki tej funkcji, gdy uruchomisz program z argumentami, np. `clojure.exe program.clj argument1 argument2`, zobaczysz na ekranie:

```Clojure
argument1
argument2
```

To tylko jedno z wielu zastosowań czytania argumentów z wiersza poleceń. Możesz na ich podstawie wykonać różne operacje, przekazać je do innych funkcji, czy też ustawić różne opcje dla programu.

## Glebokie zanurzenie

Zanim przystąpisz do czytania argumentów z wiersza poleceń, dobrze jest zrozumieć, jak są one przekazywane do Twojego programu. Każdy argument jest oddzielony od innych spacją i jest traktowany jako odrębny ciąg znaków. To oznacza, że jeśli chcesz, aby Twój program rozpoznawał argumenty z nawiasami lub cudzysłowami, musisz odpowiednio je obsłużyć.

Możesz również dodać do swojego programu opcję obsługi błędów w przypadku, gdy użytkownik nie podał żadnych argumentów, albo podał ich za dużo. W ten sposób Twój program będzie bardziej niezawodny i odpowiednio reagować na różne przypadki.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o czytaniu argumentów z wiersza poleceń w Clojure, polecam przeczytać te artykuły:

- [Czytanie argumentów w Clojure](https://clojure.org/reference/deps_and_cli#_command_line_arguments) 
- [Argumenty z wiersza poleceń w akcji](https://www.braveclojure.com/functional-programming/)