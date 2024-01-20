---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Pisanie do standardowego błędu (stderr) to sposób na wyprowadzanie komunikatów o błędach i innych ważnych informacji diagnostycznych z programu. Programiści używają `stderr` do oddzielania normalnych danych wyjściowych od błędów, co ułatwia debugowanie i przetwarzanie logów.

## How to: (Jak to zrobić?)
W Clojure do pisania do `stderr` używa się funkcji `binding` z `*err*`. Oto krótki przykład wypisujący komunikat błędu:

```clojure
(binding [*out* *err*]
  (println "To jest błąd!"))
```

Wyjście będzie widoczne w stderr, nie będzie jednak zwracane jako wartość funkcji.

Inny sposób to użycie funkcji `java.io.PrintWriter`:

```clojure
(import 'java.io.PrintWriter)

(let [stderr-writer (PrintWriter. *err* true)]
  (.println stderr-writer "To jest błąd!"))
```

Tym razem, wykorzystaliśmy klasę PrintWriter do stworzenia buforowanego obiektu piszącego do `stderr`.

## Deep Dive (Dogłębna analiza)
W historii programowania, idea dwóch różnych strumieni wyjście standardowe (stdout) i błąd standardowy (stderr) powstała by rozróżniać dane wyjściowe aplikacji od komunikatów o błędach. W Clojure, standardowe wiązania *out* i *err* reprezentują te dwa strumienie.

Alternatywą dla bezpośredniego pisania do stderr jest użycie bibliotek logujących, które zapewniają większą elastyczność i konfigurowalność. 

Bezpośrednie pisanie do `stderr` w Clojure jest realizowane przez sprawdzanie i przekierowanie *out*, które jest dynamicznym symbolem. Oznacza to, że moze być ono przypisane lokalnie w danym wątku.

## See Also (Zobacz także)
- Official Clojure Documentation on I/O: [https://clojure.org/reference/java_interop#_java_io](https://clojure.org/reference/java_interop#_java_io)
- Clojure `core` API reference for `*err*`: [https://clojuredocs.org/clojure.core/*err*](https://clojuredocs.org/clojure.core/*err*)