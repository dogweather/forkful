---
date: 2024-01-20 17:40:10.029389-07:00
description: "Jak to zrobi\u0107: W starszych wersjach j\u0119zyk\xF3w programowania,\
  \ tworzenie pliku tymczasowego by\u0142o mniej bezpo\u015Brednie, wymagaj\u0105\
  c manualnego zarz\u0105dzania\u2026"
lastmod: '2024-04-05T22:50:49.324265-06:00'
model: gpt-4-1106-preview
summary: "W starszych wersjach j\u0119zyk\xF3w programowania, tworzenie pliku tymczasowego\
  \ by\u0142o mniej bezpo\u015Brednie, wymagaj\u0105c manualnego zarz\u0105dzania\
  \ \u015Bcie\u017Ckami i uprawnieniami."
title: Tworzenie pliku tymczasowego
weight: 21
---

## Jak to zrobić:
```clojure
(require '[clojure.java.io :as io])

(defn create-temp-file [prefix suffix]
  (.createTempFile (io/file (System/getProperty "java.io.tmpdir")) prefix suffix))

;; Użycie:
(def temp-file (create-temp-file "example" ".tmp"))

;; Sprawdzenie:
(println "Tymczasowy plik został stworzony: " (.getPath temp-file))

;; Kiedy skończysz, usuń plik:
(.delete temp-file)
```
Przykładowe wyjście:
```
Tymczasowy plik został stworzony: /tmp/example4353434467984643904.tmp
```

## Wgłębienie się
W starszych wersjach języków programowania, tworzenie pliku tymczasowego było mniej bezpośrednie, wymagając manualnego zarządzania ścieżkami i uprawnieniami. Clojure, korzystając z Java Interop, pozwala na prostsze tworzenie tymczasowych plików dzięki wbudowanym funkcjom Javy. Alternatywy to tworzenie plików w określonym katalogu, ale pamiętaj - zarządzanie tymczasowymi plikami jest ważne, by uniknąć wycieków zasobów. Implementacja Clojure załatwia wiele problemów, włączając automatyczne generowanie unikalnych nazw plików i opcję usunięcia pliku po wyłączeniu JVM (Java Virtual Machine), jeśli korzystasz z `deleteOnExit`.

## Zobacz także:
- Dokumentacja Clojure `java.io`: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Dokumentacja klasy `java.io.File`: [https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/File.html](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/File.html)
