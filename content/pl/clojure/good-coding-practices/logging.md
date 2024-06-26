---
date: 2024-01-26 01:02:29.105636-07:00
description: "Jak to zrobi\u0107: Clojure opiera si\u0119 o udogodnienia logowania\
  \ Javy, ale mo\u017Cesz si\u0119 do nich dobra\u0107 w bardziej idiomatyczny dla\
  \ Clojure spos\xF3b. Przyjrzyjmy\u2026"
lastmod: '2024-03-13T22:44:35.002526-06:00'
model: gpt-4-1106-preview
summary: "Clojure opiera si\u0119 o udogodnienia logowania Javy, ale mo\u017Cesz si\u0119\
  \ do nich dobra\u0107 w bardziej idiomatyczny dla Clojure spos\xF3b."
title: "Rejestrowanie zdarze\u0144"
weight: 17
---

## Jak to zrobić:
Clojure opiera się o udogodnienia logowania Javy, ale możesz się do nich dobrać w bardziej idiomatyczny dla Clojure sposób. Przyjrzyjmy się, jak możesz użyć `clojure.tools.logging`, które dostarcza prostej abstrakcji dla kilku frameworków logujących:

Po pierwsze, dodaj zależności dla `clojure.tools.logging` i implementacji logowania takiej jak `log4j` w twoim `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Teraz zalogujmy kilka wiadomości:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Rozpoczynam intensywne obliczenia...")
  (Thread/sleep 3000) ; Symulacja długiego obliczenia
  (log/info "Obliczenia zakończone. Odpowiedź to 42.")
  42)

(compute-answer-to-everything)
```
Domyślnie wynik nie pokaże wiadomości `DEBUG`, ponieważ poziomy logowania są zazwyczaj ustawione na `INFO`:

```
INFO  [twoja-przestrzeń-nazw] - Obliczenia zakończone. Odpowiedź to 42.
```

Możesz skonfigurować poziomy logowania i dodatki (appenders) w pliku `log4j.properties`, aby uzyskać bardziej szczegółowe wyjście, jeśli jest to potrzebne.

## Dogłębna analiza
`clojure.tools.logging` w Clojure istnieje już od jakiegoś czasu i służy jako most między kodem Clojure a światem logowania Java. W historii, Java przeszła przez kilka iteracji i bibliotek do logowania, takich jak wbudowane API logowania Java, `log4j`, `slf4j` i `logback`.

W Clojure, chociaż możesz bezpośrednio używać frameworków logowania Java, `clojure.tools.logging` wykrywa i deleguje do tego frameworku logowania, który znajdzie w twojej ścieżce klas (classpath), oszczędzając ci konieczności ściślego powiązania z konkretną implementacją. To może pomóc utrzymać twój kod Clojure bardziej przenośnym i modułowym.

Alternatywy dla `clojure.tools.logging` w ekosystemie Clojure obejmują biblioteki takie jak `timbre`, która jest czystą biblioteką logowania Clojure z funkcjami takimi jak rotacja logów, filtrowanie i asynchroniczne logowanie prosto "z pudełka".

Szczegóły implementacji są kluczowe jeśli chodzi o logowanie w wielowątkowym środowisku jakim jest Clojure. Tutaj, niemutowalność i zarządzanie efektami ubocznymi zapewniają wyraźne korzyści. Logowanie, jako efekt uboczny, powinno być obsługiwane ostrożnie, aby uniknąć wąskich gardeł wydajnościowych i zapewnić bezpieczeństwo wątków, o co większość frameworków logowania Java już dba.

Na koniec, rozważ logowanie strukturalne, gdzie logi są zapisywane jako strukturalne dane (takie jak JSON). Może to być niezmiernie przydatne do późniejszej analizy i przetwarzania, szczególnie przy obróbce dużych, rozproszonych systemów.

## Zobacz również
Jeśli szukasz więcej informacji, rozważ sprawdzenie tych zasobów:

- Dokumentacja Clojure Tools Logging: https://github.com/clojure/tools.logging
- Timbre, biblioteka logowania Clojure: https://github.com/ptaoussanis/timbre
- Konfigurowanie Log4J w Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Instrukcja Logback dla zaawansowanych konfiguracji: http://logback.qos.ch/manual/
- Przewodnik po logowaniu strukturalnym w Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
