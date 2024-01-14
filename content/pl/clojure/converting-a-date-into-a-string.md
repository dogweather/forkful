---
title:                "Clojure: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym narzędziem w wielu programach, szczególnie tych związanych z obsługą danych lub wyświetlaniem informacji użytkownikowi. W tym artykule dowiesz się, jak za pomocą języka Clojure przekonwertować datę na czytelny dla człowieka format.

## Jak To Zrobić

Pierwszym krokiem jest zaimportowanie modułu Java.time, który jest odpowiedzialny za zarządzanie datami w języku Clojure.

```Clojure
(require '[java.time :as time])
```

Następnie możemy utworzyć obiekt daty za pomocą funkcji `now` z modułu `LocalDate`.

```Clojure
(def data-dzisiejsza (time/LocalDate/now))
```

Teraz pozostaje tylko przekonwertować tę datę na ciąg znaków za pomocą funkcji `toString`.

```Clojure
(def data-tekstowa (.toString data-dzisiejsza))
```

W efekcie otrzymujemy datę w formacie `rrrr-mm-dd`, na przykład "2021-02-15".

## Rzut Okiem

Każda data w języku Clojure jest przechowywana jako obiekt, więc w przypadku konwersji na ciąg znaków musimy uważać na odpowiednie formatowanie. Jeśli chcemy otrzymać datę w innym formacie niż "rrrr-mm-dd", możemy skorzystać z metody `format` z modułu `LocalDate`.

```Clojure
(def data-formatowana (.format data-dzisiejsza (time/format "dd.MM.rrrr")))
```

Tutaj określamy pożądany format za pomocą specjalnych znaków, na przykład "dd.MM.rrrr" oznacza datę w formacie "dzisiaj.miesiąc.rok", czyli "15.02.2021".

## Zanurzenie W Temat

W języku Clojure można również skonwertować datę i czas do jednego obiektu za pomocą modułu `LocalDateTime`.

```Clojure
(def data-i-czas (time/LocalDateTime/now))
```

Pozwala to na jeszcze większą kontrolę nad wyświetlanymi informacjami, na przykład można wyświetlić czas wraz z datą za pomocą formatu "rrrr-mm-dd hh:mm:ss".

```Clojure
(def data-i-czas-formatowane (.format data-i-czas (time/format "rrrr-mm-dd hh:mm:ss")))
```

W ten sposób otrzymujemy datę w formacie "2021-02-15 15:47:25".

## Zobacz Również

Jeśli chcesz dowiedzieć się więcej o pracy z datami w języku Clojure, polecamy zapoznać się z dokumentacją modułu Java.time oraz skorzystać z następujących linków:

- [Java.time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Przykłady formatowania dat w Clojure](https://clojuredocs.org/clojure.java-time/format)
- [Poradnik dotyczący pracy z datami i czasem w Clojure](https://purelyfunctional.tv/guide/clojure-date-time/)