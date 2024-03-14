---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:43.605468-07:00
description: "Wyra\u017Cenia regularne to pot\u0119\u017Cne narz\u0119dzie do dopasowywania\
  \ wzorc\xF3w i manipulowania danymi, niezb\u0119dne w zadaniach przetwarzania tekstu,\
  \ takich jak walidacja\u2026"
lastmod: '2024-03-13T22:44:34.984249-06:00'
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne to pot\u0119\u017Cne narz\u0119dzie do dopasowywania\
  \ wzorc\xF3w i manipulowania danymi, niezb\u0119dne w zadaniach przetwarzania tekstu,\
  \ takich jak walidacja\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne to potężne narzędzie do dopasowywania wzorców i manipulowania danymi, niezbędne w zadaniach przetwarzania tekstu, takich jak walidacja danych wejściowych, wyszukiwanie i zamiana tekstu. Programiści używają ich intensywnie do efektywnego i zwięzłego obsługiwania złożonych zadań związanych z analizą ciągów znaków i walidacją danych.

## Jak to zrobić:
Clojure, pozostając wiernym swoim korzeniom z rodziny Lisp, oferuje bogaty zestaw funkcji, które bezproblemowo współpracują z możliwościami wyrażeń regularnych Javy. Oto jak możesz z nich korzystać:

### Podstawowe dopasowywanie
Aby sprawdzić, czy ciąg znaków pasuje do wzorca, użyj `re-matches`. Zwraca całe dopasowanie w przypadku sukcesu lub `nil` w przeciwnym razie.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Wyszukiwanie wzorców
Aby znaleźć pierwsze wystąpienie wzorca, `re-find` jest funkcją, do której powinieneś się zwrócić:

```clojure
(re-find #"\d+" "Zamówienie 123")  ;=> "123"
```

### Grupy przechwytujące
Użyj `re-find` wraz z nawiasami w swoim wzorcu, aby przechwycić grupy:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Telefon: 123-4567")]
  (println "Kod obszaru:" area "Kod:" code))
;; Wyjście: Kod obszaru: nil Kod: 123
```

### Wyszukiwanie globalne (Znajdź wszystkie dopasowania)
Clojure nie ma wbudowanego globalnego wyszukiwania, jak niektóre języki. Zamiast tego użyj `re-seq` aby otrzymać leniwą sekwencję wszystkich dopasowań:

```clojure
(re-seq #"\d+" "id: 123, ilość: 456")  ;=> ("123" "456")
```

### Dzielenie ciągów znaków
Aby podzielić ciąg znaków na podstawie wzorca, użyj `clojure.string/split`:

```clojure
(clojure.string/split "Jan,Kowalski,30" #",")  ;=> ["Jan" "Kowalski" "30"]
```

### Zamiana
Zamień części ciągu znaków pasujące do wzorca za pomocą `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "RRRR")  ;=> "RRRR-04-01"
```

### Biblioteki firm trzecich
Chociaż wbudowane wsparcie Clojure wystarcza w większości przypadków, dla bardziej złożonych scenariuszy, rozważ użycie bibliotek takich jak `clojure.spec` dla solidnej walidacji danych i `reagent` do reaktywnej manipulacji DOM w aplikacjach internetowych z wykorzystaniem wyrażeń regularnych do routingu i walidacji danych wejściowych.

```clojure
;; Przykład użycia clojure.spec do walidacji adresu email
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

Pamiętaj, że choć wyrażenia regularne są potężne, mogą również sprawić, że kod będzie trudny do odczytania i utrzymania. Używaj ich z rozwagą i zawsze rozważ prostsze funkcje manipulacji ciągami znaków, jeśli jest to możliwe.
