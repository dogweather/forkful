---
title:                "Pisanie do standardowego błędu"
html_title:           "Clojure: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zapisywanie do standardowego błędu jest procesem, który polega na przesyłaniu informacji o błędach lub ostrzeżeniach na standardowe wyjście błędu zamiast na standardowe wyjście. Programiści często robią to, aby łatwiej monitorować i debugować swoje programy.

## Jak to zrobić:

Kodując w języku Clojure, można użyć funkcji `(System/err (str "Tekst, który chcesz zapisać do standardowego błędu"))`, aby przekierować informacje o błędach na standardowe wyjście błędu. Poniżej znajduje się przykładowy kod i wyjście, który pokazuje działanie tej funkcji.

```Clojure
(defn throw-error []
  (System/err (str "Wystąpił błąd!")))
  
(throw-error)
```

Output:
```
Wystąpił błąd!
```

## Głębszy Przegląd:

Zapisywanie do standardowego błędu jest częstym sposobem na komunikowanie informacji o błędach w programach. Praktyka ta pochodzi z Unixa, gdzie standardowe wyjście błędu jest oddzielone od standardowego wyjścia, co ułatwia debugowanie i przesyłanie informacji na zewnątrz. Alternatywą dla tego podejścia jest korzystanie z bibliotek do obsługi błędów, takich jak Sentry lub Log4j. Implementacja zapisywania do standardowego błędu w języku Clojure jest prostym procesem, ponieważ język ten jest zbudowany na platformie JVM, która obsługuje standardowe wyjście błędu.

## Zobacz też:

- Oficjalna dokumentacja języka Clojure: https://clojure.org
- Dokumentacja na temat standardowych strumieni w języku Java: https://docs.oracle.com/javase/tutorial/essential/io/stream.html