---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:21.992312-07:00
description: "Pobranie bie\u017C\u0105cej daty w programowaniu jest kluczowe z wielu\
  \ przyczyn, w tym logowania, oznaczania czasu zdarze\u0144 i planowania zada\u0144\
  . W Clojure, dialekcie\u2026"
lastmod: '2024-03-13T22:44:35.007413-06:00'
model: gpt-4-0125-preview
summary: "Pobranie bie\u017C\u0105cej daty w programowaniu jest kluczowe z wielu przyczyn,\
  \ w tym logowania, oznaczania czasu zdarze\u0144 i planowania zada\u0144."
title: Pobieranie aktualnej daty
weight: 29
---

## Co i dlaczego?
Pobranie bieżącej daty w programowaniu jest kluczowe z wielu przyczyn, w tym logowania, oznaczania czasu zdarzeń i planowania zadań. W Clojure, dialekcie Lispu na JVM, to zadanie korzysta z możliwości interop Java, umożliwiając bezproblemowy dostęp do bogatego Java Date-Time API.

## Jak to zrobić:

### Korzystając z Java Interop
Bezproblemowa interoperacyjność Clojure z Javą umożliwia bezpośrednie korzystanie z Java Date-Time API. Oto jak można uzyskać bieżącą datę:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Przykładowe wyjście
(get-current-date) ; "2023-04-15"
```

### Korzystając z biblioteki clj-time
Dla bardziej idiomatycznego rozwiązania w Clojure, możesz zdecydować się na bibliotekę `clj-time`, opakowanie wokół Joda-Time, chociaż dla większości nowych projektów zalecane jest wbudowane Java 8 Date-Time API. Jednakże, jeśli preferujesz lub wymagasz `clj-time`:

Najpierw dodaj `clj-time` do zależności twojego projektu. W twoim pliku `project.clj`, dołącz:

```clojure
[clj-time "0.15.2"]
```

Następnie, użyj tego do uzyskania bieżącej daty:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Przykładowe wyjście
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Obie metody zapewniają szybki, skuteczny sposób na uzyskanie bieżącej daty w Clojure, wykorzystując moc leżącej u podstaw platformy Java lub wygodę biblioteki specyficznej dla Clojure.
