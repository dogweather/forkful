---
title:                "Rejestrowanie zdarzeń"
aliases:
- /pl/clojure/logging/
date:                  2024-01-26T01:02:29.105636-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie to w zasadzie oprogramowaniowy odpowiednik dziennika pokładowego statku; jest to sposób na rejestrowanie zdarzeń, które mają miejsce podczas działania aplikacji. Programiści robią to aby śledzić te zdarzenia w celu debugowania, tworzenia ścieżek audytu, lub zdobycia wglądu w zachowanie systemu w środowisku produkcyjnym.

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
