---
aliases:
- /pl/clojure/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:20.801101-07:00
description: "Sprawdzanie czy katalog istnieje w Clojure polega na weryfikacji obecno\u015B\
  ci katalogu systemu plik\xF3w z poziomu aplikacji Clojure. Zadanie to jest kluczowe\u2026"
lastmod: 2024-02-18 23:08:49.278740
model: gpt-4-0125-preview
summary: "Sprawdzanie czy katalog istnieje w Clojure polega na weryfikacji obecno\u015B\
  ci katalogu systemu plik\xF3w z poziomu aplikacji Clojure. Zadanie to jest kluczowe\u2026"
title: Sprawdzanie, czy katalog istnieje
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie czy katalog istnieje w Clojure polega na weryfikacji obecności katalogu systemu plików z poziomu aplikacji Clojure. Zadanie to jest kluczowe dla operacji na plikach, aby uniknąć błędów podczas odczytu z lub zapisu do katalogów, które mogą nie istnieć, zapewniając niezawodne i wolne od błędów wykonanie kodu.

## Jak to zrobić:
Clojure, jako język działający na JVM, może wykorzystać klasę `java.io.File` z Javy do tego celu. Nie potrzebujesz żadnej biblioteki stron trzecich dla tak podstawowej operacji. Oto jak możesz to zrobić:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; Przykład użycia
(println (directory-exists? "/ścieżka/do/twojego/katalogu")) ;; true albo false
```

Ta funkcja, `directory-exists?`, przyjmuje ścieżkę katalogu jako ciąg znaków i zwraca `true`, jeśli katalog istnieje, oraz `false` w przeciwnym razie. Jest to osiągane poprzez utworzenie obiektu `File` ze ścieżką katalogu, a następnie wywołanie metody `.exists` na tym obiekcie.

Oprócz bezpośredniego wykorzystania Javy, możesz użyć bibliotek Clojure, które abstrahują część boilerplate'u Javy. Jedną z takich bibliotek jest `clojure.java.io`. Jednakże, do sprawdzenia czy katalog istnieje, nadal używałbyś klasy `File`, ale możesz uznać bibliotekę za przydatną do innych operacji na plikach. Przykład:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; Przykład użycia
(println (directory-exists?-clojure "/inną/ścieżkę/do/sprawdzenia")) ;; true albo false
```

Ta wersja jest dość podobna, ale używa funkcji Clojure `io/file` do stworzenia obiektu `File`. Ta metoda bardziej naturalnie wpisuje się w bazy kodu Clojure przez wykorzystanie biblioteki Clojure do operacji IO, zamiast bezpośredniego interfejsowania się z klasami Javy.
