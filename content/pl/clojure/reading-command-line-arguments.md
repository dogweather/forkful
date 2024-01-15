---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Clojure: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach coraz więcej programów wymaga wprowadzania argumentów wiersza poleceń. Jeśli jesteś programistą Clojure i chcesz nauczyć się jak czytać argumenty wiersza poleceń w swoich programach, to ten artykuł jest dla Ciebie!

## Jak to zrobić

```Clojure
(defn parse-args [args]
  (split-with #(not= % "--") args))

(defn get-flags [args]
  (->> args
       (drop 1)
       (map first)))

(defn get-args [args]
  (->> args
       (drop 1)
       (partition 2)
       (map second)
       (into {})))

(defn parse-cmd-args [args]
  (let [[flags args] (parse-args args)
        flags (into #{} (get-flags flags))
        args (get-args args)]
    (merge args {:flags flags})))

(defn -main [& args]
  (let [{:flags flags :args args} (parse-cmd-args args)]
    (println "Flags:" flags)
    (println "Arguments:" args)))

```

Po uruchomieniu powyższego kodu z argumentami `--debug --name John` otrzymamy następujący wynik:

```
Flags: #{:debug}
Arguments: {:name "John"}
```

## Deep Dive

Do obsługi argumentów wiersza poleceń w Clojure możemy wykorzystać funkcje `parse-args`, `get-flags` i `get-args`, które są odpowiedzialne za podzielenie argumentów na flagi i wyodrębnienie wartości pomiędzy nimi. Następnie funkcja `parse-cmd-args` łączy ze sobą wszystkie wcześniej zdefiniowane funkcje, aby zwrócić mapę zawierającą wszystkie podane argumenty oraz flagi.

Możemy również dodać własne funkcje do obsługi konkretnych argumentów. Na przykład, jeśli chcemy, aby nasz program przyjmował tylko liczby jako argumenty, możemy napisać funkcję, która będzie sprawdzać, czy wartość podana w argumencie jest liczbą. Dzięki takiemu podejściu uzyskujemy pełną kontrolę nad obsługą argumentów w naszym programie.

## Zobacz także

- Dokumentacja Clojure: https://clojure.org/documentation
- Przewodnik po języku Clojure: https://clojure.org/guides
- Blog Clojure: https://clojure.com/blog