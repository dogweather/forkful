---
title:                "Korzystanie z tablic asocjacyjnych"
date:                  2024-01-30T19:11:10.213793-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, znane także jako mapy haszujące, w Clojure pozwalają przechowywać i pobierać dane za pomocą par klucz-wartość. Są one chętnie wykorzystywane do zarządzania danymi strukturalnymi, co ułatwia szybki dostęp do określonych elementów bez potrzeby iteracji przez listę.

## Jak to zrobić:

W Clojure tworzenie i manipulowanie tablicami asocjacyjnymi (mapami haszującymi) jest proste. Zagłębmy się w przykłady.

Aby utworzyć mapę haszującą:

```clojure
(def my-map {:name "Alex" :age 30})
```

Możesz pobrać wartość, określając jej klucz:

```clojure
(get my-map :name)
;; "Alex"
```
Lub, bardziej idiomatycznie, możesz użyć klucza jako funkcji:

```clojure
(:name my-map)
;; "Alex"
```

Dodawanie lub aktualizowanie wpisów jest proste:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Do usuwania kluczy użyj `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Aby iterować po mapie:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

I dla warunkowego dostępu, `find` zwraca parę klucz-wartość, jeśli klucz istnieje:

```clojure
(find my-map :age)
;; [:age 30]
```

## Głębsze zanurzenie

Tablice asocjacyjne w Clojure, nazywane również mapami haszującymi, są niesamowicie wszechstronne i efektywne w zarządzaniu danymi opartymi na parach klucz-wartość. Są częścią bogatej biblioteki kolekcji Clojure, głęboko zakorzenionej w filozofii immutability (niemutowalności) i programowania funkcyjnego. W przeciwieństwie do tablic czy list, które wymagają złożoności czasowej O(n) do dostępu do elementów, mapy haszujące zapewniają prawie stałą złożoność czasową dostępu, co sprawia, że są wysoce efektywne dla operacji wyszukiwania.

Można argumentować, że wektory w Clojure mogłyby służyć podobnemu celowi poprzez dostęp zindeksowany, ale mapy haszujące świecą, gdy chodzi o radzenie sobie z danymi niesekwencyjnymi i oznakowanymi, gdzie klucz dostarcza znaczący deskryptor zamiast arbitralnego indeksu.

Unikalne dla Clojure (i jego dziedzictwa Lispa), tablice asocjacyjne są obywatelami pierwszej klasy, co oznacza, że mogą być bezpośrednio manipulowane, przekazywane do funkcji i więcej, bez potrzeby specjalnej składni czy metod dostępu. Ta decyzja projektowa wzmacnia nacisk Clojure na prostotę i moc.

Chociaż mapy haszujące są niesamowicie użyteczne, warto wspomnieć, że dla bardzo dużych zbiorów danych lub scenariuszy, w których klucze są bardzo dynamiczne (stałe dodawanie i usuwanie), alternatywne struktury danych lub bazy danych mogą oferować lepszą wydajność i elastyczność. Jednakże, dla większości typowych przypadków użycia w ramach aplikacji Clojure, tablice asocjacyjne zapewniają solidny i efektywny sposób zarządzania danymi.
