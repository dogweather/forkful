---
title:    "Clojure: Łączenie napisów"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego
W dzisiejszym wpisie przyjrzymy się, dlaczego warto uczyć się łączenia ciągów w języku Clojure. Jest to niezbędna umiejętność w każdym języku programowania, włącznie z Clojure. Pozwala na tworzenie bardziej interaktywnych i złożonych programów, a także ułatwia pracę z tekstami.

## Jak to zrobić
W Clojure, łączenie ciągów można wykonać za pomocą funkcji `str`, która w swoim argumencie przyjmuje dowolne ciągi znaków i zwraca je połączone w jedną dłuższą linię. Spójrzmy na przykładowy kod:

```Clojure
(str "Witaj " "świecie!" " Jestem " "Clojure.")
```

Po uruchomieniu tego kodu otrzymamy następujący wynik:

```Clojure
"Witaj świecie! Jestem Clojure."
```

Funkcja `str` jest bardzo elastyczna, ponieważ może przyjmować różne typy danych, takie jak liczby czy kolekcje. Jeśli chcemy połączyć więcej niż dwa ciągi, możemy po prostu dodać je jako kolejne argumenty do funkcji `str`.

## Głębsze spojrzenie
W Clojure istnieje również funkcja `str-join`, która działa podobnie do funkcji `str`, ale pozwala na bardziej zaawansowane łączenie ciągów. Możemy określić separator, który będzie oddzielał poszczególne ciągi oraz wskazać, które elementy z kolekcji chcemy połączyć. Przykładowo:

```Clojure
(str/join ", " ["Jabłko" "Banan" "Pomarańcza"])
```

Wynik wyglądałby następująco:

```Clojure
"Jabłko, Banan, Pomarańcza"
```

Ponadto, w Clojure można również łączyć ciągi z wykorzystaniem operatora `+`, ale jest to mniej wydajna metoda, ponieważ tworzy ona nowe ciągi w pamięci za każdym razem, gdy jest wywoływana. Dlatego zalecamy korzystanie z funkcji `str` lub `str-join` w przypadku łączenia ciągów.

## Zobacz także
- [Oficjalna dokumentacja Clojure](https://clojure.org/index)
- [Poradnik dla początkujących w języku Clojure (po polsku)](https://jakubarnold.pl/clojure/kurs-podstawy/)
- [Dokumentacja funkcji `str`](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/str)