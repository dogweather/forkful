---
title:    "Clojure: Tworzenie tymczasowego pliku"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowego pliku jest częstym zagadnieniem podczas programowania w języku Clojure. Często potrzebujemy przechować dane tymczasowo, np. przy przetwarzaniu dużych plików lub testowaniu kodu. W tym artykule zajmiemy się tworzeniem tymczasowych plików w języku Clojure.

## Jak to zrobić?

Tworzenie tymczasowych plików w Clojure jest bardzo proste. Możemy skorzystać z funkcji "with-open", która otwiera plik i wykonuje określone działania, a następnie automatycznie zamyka plik po zakończeniu tych działań. Oto przykładowy kod:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "prefix_" ".txt")]
  (println "Plik tymczasowy został utworzony.")
  (println "Jego ścieżka to: " (.getPath temp-file)))
```

Tworzymy tymczasowy plik za pomocą funkcji "createTempFile" dostępnej w bibliotece Java. Funkcja ta przyjmuje dwa argumenty - prefix i suffix (oznaczenie używane w nazwie pliku) i zwraca obiekt typu "java.io.File". W naszym przykładzie dodatkowo wypisujemy informacje o utworzonym pliku.

Możemy także ustawić wybraną lokalizację dla pliku tymczasowego, podając ścieżkę bezwzględną jako trzeci argument funkcji "createTempFile".

## Podgląd

Warto zauważyć, że plik tymczasowy jest automatycznie usuwany po zakończeniu działania funkcji "with-open". Jednak jeśli chcemy zachować plik na stałe, możemy przechować go w zmiennej i wykorzystać go później w kodzie.

Istnieje także możliwość ręcznego usunięcia pliku tymczasowego za pomocą funkcji "java.io.File/delete", która jako argument przyjmuje obiekt pliku.

## Przypominamy o bezpieczeństwie!

Podczas pracy z tymczasowymi plikami należy pamiętać o bezpieczeństwie. Należy dbać o to, aby nie tworzyć potencjalnych luk w zabezpieczeniach naszego systemu. Dlatego też zalecamy używanie prefixów i suffixów, które są unikatowe i nie są łatwe do odgadnięcia przez potencjalnego atakującego.

## Zobacz także

- Dokumentacja funkcji "with-open": https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/with-open
- Przykładowy kod tworzenia tymczasowego pliku w języku Clojure: https://learnxinyminutes.com/docs/clojure/