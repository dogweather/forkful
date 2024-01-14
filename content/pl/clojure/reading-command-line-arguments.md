---
title:    "Clojure: Odczytanie argumentów wiersza poleceń"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w Clojure może wydawać się złożone i trudne do nauki dla wielu osób. Jednak znajomość podstawowych funkcji i narzędzi może sprawić, że praca z tym językiem stanie się łatwiejsza i bardziej efektywna. W tym artykule dowiesz się, dlaczego warto poznać jak czytać argumenty wiersza poleceń.

## Jak To Zrobić

W języku Clojure, argumenty wiersza poleceń można odczytać z użyciem funkcji `*command-line-args*`. Najpierw należy zaimplementować tę funkcję w swoim kodzie, aby móc uzyskać dostęp do argumentów wpisanych podczas uruchamiania programu. Następnie można je wykorzystać w celu manipulacji i przetwarzania danych w naszej aplikacji.

Poniżej znajdziesz przykładowy kod wykorzystujący funkcję `*command-line-args*` do wyświetlenia argumentów wpisanych przez użytkownika oraz sumowania liczb przekazanych jako argumenty:

```Clojure
(defn read-command-line-args []
  (println "Wprowadzone argumenty:")
  (let [args *command-line-args*]
    (doseq [arg args]
      (println arg))
    (let [num-args (count args)
          sum (apply + (map #(Integer/parseInt %) args))]
      (println "Suma argumentów wynosi:" sum))))

(read-command-line-args)
```

Dla przykładu, jeśli wprowadzimy wiersz poleceń `lein run 1 2 3`, to otrzymamy następujący wynik:

```Clojure
Wprowadzone argumenty:
1
2
3
Suma argumentów wynosi: 6
```

## Deep Dive

Jedną z głównych zalet czytania argumentów wiersza poleceń jest możliwość dostosowania działania naszej aplikacji bez konieczności zmiany kodu. W ten sposób, za pomocą różnych argumentów, użytkownik może wywoływać różne funkcje lub przekazywać różnego rodzaju dane do aplikacji.

W języku Clojure można również korzystać z biblioteki `clojure.tools.cli` w celu łatwiejszej obsługi argumentów wiersza poleceń. Dzięki niej możemy używać flag, opcji i argumentów pozycyjnych w naszych aplikacjach w bardziej przejrzysty i elastyczny sposób.

## Zobacz również

- Oficjalna dokumentacja do Clojure: (https://clojure.org/)
- Przewodnik po języku Clojure: (https://www.braveclojure.com/getting-started/)
- Dokumentacja biblioteki `clojure.tools.cli`: (https://clojure.github.io/tools.cli/)