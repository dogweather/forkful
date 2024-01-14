---
title:                "Clojure: Praca z json"
simple_title:         "Praca z json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest jednym z najpopularniejszych formatów danych w dzisiejszym świecie programowania. Jest to popularna metoda przechowywania i przesyłania danych, szczególnie w aplikacjach internetowych. Praca z JSON jest nieodłączną częścią wielu projektów, dlatego warto poznać podstawy tego formatu danych.

## Jak

```Clojure
; Tworzenie prostego JSON obiektu
(def moj-json {:imie "Kasia"
               :wiek 27
               :zainteresowania ["programowanie" "podroze" "gotowanie"]})

; Konwersja do formatu tekstowego
(clojure.data.json/write-str moj-json)

; Wyjście:
; {"imie":"Kasia","wiek":27,"zainteresowania":["programowanie","podroze","gotowanie"]}
```

```Clojure
; Wczytywanie JSON z zewnętrznego pliku
(require '[cheshire.core :as json])
(def dane-json (json/parse-string (slurp "moj-plik.json" :encoding "UTF-8")))

; Pobieranie wartości z obiektu
(println (get-in dane-json [:dane :produkt :cena]))

; Wyjście:
; 49.99
```

```Clojure
; Modyfikacja istniejącego JSON obiektu
(defn dodaj-kategorie [dane kategorie]
  (update-in dane [:produkty] #(conj % :kategorie kategorie)))

(def moj-nowy-json (dodaj-kategorie dane-json ["elektronika" "gry komputerowe"]))
(println (get-in moj-nowy-json [:produkty 2 :kategorie]))

; Wyjście:
; ["muzyka" "elektronika" "gry komputerowe"]
```

## Deep Dive

Pojęcie JSON pojawiło się w latach 90. XX wieku jako alternatywny format dla XML. Jest to prosty, lekki i czytelny format, który jest łatwy do zrozumienia przez ludzi, jak i przez komputery. Struktura JSON’a składa się z par klucz-wartość, gdzie wartości mogą być liczbami, tekstami, tablicami lub innymi obiektami JSON. Praca z tym formatem jest podobna do pracy z mapami w Clojure, co sprawia, że jest to jedna z najłatwiejszych rzeczy w języku.

## Zobacz również

- [Clojure Cookbook - JSON](https://clojure.org/api/cheatsheet)
- [Clojure Data JSON](https://github.com/clojure/data.json)
- [A Beginners Guide to JSON](https://www.digitalocean.com/community/tutorials/a-beginner-s-guide-to-json)