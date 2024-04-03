---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:03.998061-07:00
description: "Zapisywanie do standardowego b\u0142\u0119du (stderr) polega na kierowaniu\
  \ komunikat\xF3w o b\u0142\u0119dach i diagnostyki do strumienia stderr, oddzielnie\
  \ od standardowego\u2026"
lastmod: '2024-03-13T22:44:35.013531-06:00'
model: gpt-4-0125-preview
summary: "Zapisywanie do standardowego b\u0142\u0119du (stderr) polega na kierowaniu\
  \ komunikat\xF3w o b\u0142\u0119dach i diagnostyki do strumienia stderr, oddzielnie\
  \ od standardowego wyj\u015Bcia (stdout)."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
W Clojure można pisać do stderr używając strumienia `*err*`. Oto podstawowy przykład:

```clojure
(.write *err* "To jest komunikat o błędzie.\n")
```

Należy zauważyć, że po napisaniu wiadomości, powinieneś opróżnić strumień, aby upewnić się, że wiadomość jest natychmiast wyświetlona:

```clojure
(flush)
```

Przykładowe wyjście do stderr:
```
To jest komunikat o błędzie.
```

Jeśli zajmujesz się obsługą wyjątków, możesz chcieć wyświetlić ślady stosu do stderr. Użyj `printStackTrace` do tego celu:

```clojure
(try
  ;; Kod, który może zgłosić wyjątek
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Dla bardziej strukturalnego logowania błędów, biblioteki stron trzecich takie jak `timbre` mogą być konfigurowane do logowania do stderr. Oto podstawowa konfiguracja i użycie:

Najpierw dodaj `timbre` do swoich zależności. Następnie skonfiguruj go do używania stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Wyłącz logowanie stdout
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Wyłącz logowanie do pliku
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Włącz stderr dla błędów

(timbre/error "Wystąpił błąd podczas przetwarzania Twojego żądania.")
```

To skieruje komunikaty na poziomie błędu do stderr, czyniąc je odrębnymi od standardowego wyjścia aplikacji.
