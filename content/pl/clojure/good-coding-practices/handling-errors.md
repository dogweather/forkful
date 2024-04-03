---
date: 2024-01-26 00:51:33.578442-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w dotyczy zarz\u0105dzania nieoczekiwanymi\
  \ sytuacjami w programach \u2013 jak ochroniarz radz\u0105cy sobie z awanturnikami.\
  \ Programi\u015Bci lubi\u0105 gdy wszystko\u2026"
lastmod: '2024-03-13T22:44:35.003837-06:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w dotyczy zarz\u0105dzania nieoczekiwanymi\
  \ sytuacjami w programach \u2013 jak ochroniarz radz\u0105cy sobie z awanturnikami."
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Co i dlaczego?
Obsługa błędów dotyczy zarządzania nieoczekiwanymi sytuacjami w programach – jak ochroniarz radzący sobie z awanturnikami. Programiści lubią gdy wszystko idzie gładko; obsługa błędów pomaga utrzymać problemy w ryzach, zapewniając, że ich kod nie potknie się i nie upadnie, kiedy napotka niespodziewane przeszkody.

## Jak to zrobić:
Clojure, tak jak jego lispowe przodki, opiera się na wyjątkach do radzenia sobie z błędami. Oto jak pokazać na co cię stać, gdy sprawy przybiorą zły obrót.

Rzucanie wyjątkiem jest bezpośrednie:
```Clojure
(throw (Exception. "Ups! Coś poszło nie tak."))
```

Łapanie wyjątku, będziesz to robić często:
```Clojure
(try
  ;; ryzykowny kod
  (/ 1 0)
  (catch ArithmeticException e
    (println "Nie można dzielić przez zero!"))
  ;; blok finally wykonuje się niezależnie od wszystkiego
  (finally 
    (println "Tutaj wchodzi kod sprzątający.")))
```
Przykładowe wyjście dla powyższego bloku catch:
```
Nie można dzielić przez zero!
Tutaj wchodzi kod sprzątający.
```

Użycie `ex-info` i `ex-data` dla bogatszego kontekstu wyjątków:
```Clojure
(try
  ;; spowodowanie własnego wyjątku
  (throw (ex-info "Własny błąd" {:type :custom-failure}))
  (catch Exception e
    ;; wydobywanie danych z naszego własnego wyjątku
    (println (ex-data e))))
```
Przykładowe wyjście:
```
{:type :custom-failure}
```

## Głębsza analiza
Historia obsługi błędów w Clojure nie różni się radykalnie od innych Lispów czy nawet Javy (od której dziedziczy mechanizm `try-catch`). Jest pragmatyczna; użycie wyjątków to główna ścieżka, podobnie jak w Javie, ale Clojure oferuje funkcjonalny smak z `ex-info` i `ex-data` dla bogatszych danych o błędach.

Alternatywne metody obsługi błędów w Clojure obejmują użycie konstrukcji monadycznych, takich jak monada `either` z bibliotek takich jak `cats`, lub core.async do propagacji błędów opartej na kanałach. Jednak są to metody bardziej złożone i stosowane w konkretnych scenariuszach.

Historycznie, obsługa błędów w językach programowania ewoluowała od prostych zwracanych statusów do bardziej wyrafinowanych mechanizmów obsługi wyjątków współczesnych języków. Clojure opowiada się za prostotą i odrobiną programowania funkcyjnego, łącząc to co stare z nowym.

## Zobacz również
- Przewodnik Clojure po wyjątkach: https://clojure.org/guides/exceptions
- Biblioteka “Cats” dla bardziej funkcyjnych podejść: https://github.com/funcool/cats
- “Core.async” do programowania asynchronicznego: https://github.com/clojure/core.async
