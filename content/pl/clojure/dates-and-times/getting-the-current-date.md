---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:21.992312-07:00
description: "Jak to zrobi\u0107: Bezproblemowa interoperacyjno\u015B\u0107 Clojure\
  \ z Jav\u0105 umo\u017Cliwia bezpo\u015Brednie korzystanie z Java Date-Time API.\
  \ Oto jak mo\u017Cna uzyska\u0107 bie\u017C\u0105c\u0105 dat\u0119."
lastmod: '2024-03-13T22:44:35.007413-06:00'
model: gpt-4-0125-preview
summary: "Bezproblemowa interoperacyjno\u015B\u0107 Clojure z Jav\u0105 umo\u017C\
  liwia bezpo\u015Brednie korzystanie z Java Date-Time API."
title: Pobieranie aktualnej daty
weight: 29
---

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
