---
title:                "Pobieranie strony internetowej"
html_title:           "Clojure: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych jest niezbędne dla wielu programistów, którzy chcą analizować lub przetwarzać dane z internetu. Choć istnieje kilka narzędzi służących do tego zadania, Clojure oferuje wiele wbudowanych funkcji, które ułatwiają pobieranie stron internetowych.

## Jak to zrobić

Pobieranie strony internetowej w Clojure jest bardzo proste. Najpierw musimy zaimportować bibliotekę `clojure.java.io` do naszego projektu. Następnie używamy funkcji `slurp`, aby pobrać zawartość strony internetowej i przypisać ją do zmiennej. Na przykład:

```Clojure
(require '[clojure.java.io :as io])

(def url "https://example.com")

(def page (slurp url))
```

Teraz możemy wydrukować zawartość strony internetowej, aby upewnić się, że została pobrana poprawnie:

```Clojure
(println page)
```

Output:

```
<!DOCTYPE html>
<html>
<head>
  <title>Przykładowa strona</title>
</head>

<body>
  <h1>Witaj!</h1>
</body>
</html>
```

Jeśli chcemy przetworzyć pobraną stronę, możemy użyć wbudowanych funkcji do manipulacji danymi w formacie HTML lub wykorzystać inny język, jak na przykład ClojureScript, aby przetworzyć dane z pobranej strony.

## Deep Dive

Clojure oferuje wiele funkcji, które ułatwiają pobieranie i przetwarzanie stron internetowych. Warto zapoznać się z dokumentacją, aby poznać pełną listę dostępnych narzędzi. Między innymi, biblioteka `clojure.java.io` zawiera funkcję `reader`, która umożliwia odczytanie zawartości strony w postaci strumienia danych, co może być przydatne w przypadku przetwarzania dużych stron.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o pobieraniu stron w Clojure, polecamy zapoznanie się z tymi zasobami:

- Dokumentacja biblioteki `clojure.java.io`: https://clojuredocs.org/clojure.java.io/slurp
- Poradnik szczegółowo opisujący pobieranie stron internetowych w Clojure: https://purelyfunctional.tv/guide/how-to-download-a-web-page-in-clojure/
- Oficjalna strona języka Clojure: https://clojure.org/