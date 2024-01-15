---
title:                "Pisanie do błędu standardowego"
html_title:           "Clojure: Pisanie do błędu standardowego"
simple_title:         "Pisanie do błędu standardowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Niektórzy programiści uważają, że pisanie na standardowe wyjście błędów jest złym pomysłem, ponieważ może to przyczynić się do chaosu w konsoli i utrudnić analizowanie logów. Jednak istnieją sytuacje, w których pisanie na standardowe wyjście błędów może być przydatne, zwłaszcza podczas debugowania i testowania kodu. W tym artykule dowiesz się, dlaczego warto znać ten sposób wypisywania informacji oraz jak to zrobić w języku Clojure.

## Jak to zrobić

W Clojure, aby wypisać dane na standardowe wyjście błędów, należy użyć funkcji `(print-error some-data)`. Aby przetestować to w akcji, użyjmy poniższego kodu w REPL:

```Clojure
(print-error "Ten tekst zostanie wyświetlony na standardowym wyjściu błędów.")
```

To spowoduje wypisanie podanego tekstu na standardowe wyjście błędów:

```
Ten tekst zostanie wyświetlony na standardowym wyjściu błędów.
```

## Deep Dive

Funkcja `print-error` we właściwy sposób wymusza wypisanie danej wartości na standardowe wyjście błędów, nawet jeśli jest to skompilowany kod. Możemy również użyć funkcji `println-error`, która pozwala na dodanie znaku nowej linii na końcu wypisywanego tekstu.

Warto również wspomnieć, że w języku Clojure dostępna jest również funkcja `eprintln`, która działa podobnie jak `print-error`, ale pozwala na wprowadzenie wielu argumentów do wypisania. Możemy użyć także funkcji `pprint` do wypisania danych w bardziej czytelnej formie.

W przypadku, gdy potrzebujemy wyświetlić informacje o błędzie w naszym kodzie, można to zrobić za pomocą funkcji `eprint`, która jest podobna do `eprintln`, ale nie dodaje znaku nowej linii na końcu.

## Zobacz też

- Dokumentacja Clojure: https://clojure.org/
- Funkcja print-error: https://clojuredocs.org/clojure.core/print-error
- Funkcja println-error: https://clojuredocs.org/clojure.core/println-error
- Funkcja pprint: https://clojuredocs.org/clojure.pprint/pprint
- Funkcja eprintln: https://clojuredocs.org/clojure.core/eprintln
- Funkcja eprint: https://clojuredocs.org/clojure.core/eprint