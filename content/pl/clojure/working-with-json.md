---
title:                "Praca z JSON"
html_title:           "Clojure: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-json.md"
---

{{< edit_this_page >}}

JSON w Clojure: Jak i Dlaczego

## Co i Dlaczego?

JSON (JavaScript Object Notation) to popularny format danych wykorzystywany przez programistów do przechowywania i przesyłania informacji. Jest to tekstowy format, który jest czytelny dla człowieka, ale jednocześnie wygodny dla maszyny do przetwarzania. Programiści korzystają z JSONa, ponieważ jest lekki, przenośny, łatwy do przetwarzania i jest szeroko stosowany w różnych językach programowania i platformach.

## Jak to zrobić:

### Tworzenie JSON

Aby utworzyć JSON przy użyciu Clojure, wykorzystajmy funkcję "json/write-str", która konwertuje strukturę danych na ciąg tekstowy w formacie JSON. Przykładowy kod:

```
(require '[clojure.data.json :as json])
(json/write-str {:imie "Anna" :nazwisko "Kowalska" :wiek 30})
```

Ten kod zwróci następujący wynik:

```
"{\"name\":\"Anna\",\"surname\":\"Kowalska\",\"age\":30}"
```

### Parsowanie JSON

Aby przetworzyć dane w formacie JSON i przekształcić je w struktury danych, używamy funkcji "json/read-str". Przykładowy kod:

```
(require '[clojure.data.json :as json])
(json/read-str "{\"name\":\"Anna\",\"surname\":\"Kowalska\",\"age\":30}")
```

Ten kod zwróci następujący wynik:

```
{:name "Anna", :surname "Kowalska", :age 30}
```

## Deep Dive:

### Kontekst historyczny

JSON został opracowany w 2001 roku przez Douga Crockforda i szybko stał się popularnym formatem dla przesyłania danych w aplikacjach internetowych. Jest on oparty na języku JavaScript, ale jest szeroko stosowany we wszystkich językach programowania.

### Alternatywy

Istnieje wiele innych formatów danych, takich jak XML czy YAML, które również są wykorzystywane przez programistów. Jednak JSON jest często preferowany ze względu na swoją prostotę i kompatybilność z wieloma językami i platformami.

### Szczegóły implementacji

Clojure ma wbudowany zestaw narzędzi do przetwarzania JSON, dzięki czemu jego obsługa jest bardzo prosta i efektywna. Dane JSON można także przetwarzać za pomocą biblioteki "cheshire" lub wykorzystując biblioteki napisane w języku Java.

## Zobacz także:

- [Dokumentacja Clojure Data JSON](https://github.com/clojure/data.json)
- [Biblioteka Cheshire do przetwarzania JSON w Clojure](https://github.com/dakrone/cheshire)