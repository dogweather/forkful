---
title:                "Clojure: Analiza html"
simple_title:         "Analiza html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego
Żeby zrozumieć, dlaczego parsowanie HTML jest ważne, musimy najpierw zrozumieć, co to jest HTML. HTML (HyperText Markup Language) jest standardowym językiem używanym do tworzenia stron internetowych. Jest to język opisu struktury stron internetowych, który określa, jak powinny być wyświetlane różne elementy na stronie. Wiele witryn, takich jak media społecznościowe, sklepy internetowe i blogi, opiera się na kodzie HTML, aby wyświetlać i wyglądać odpowiednio. Dlatego umiejętność parsowania HTML jest bardzo przydatna w programowaniu aplikacji internetowych.

## Jak to zrobić
Aby parsować HTML w Clojure, musimy użyć biblioteki o nazwie Enlive, która jest dostępna za darmo w repozytorium Clojars. Najpierw musimy dodać tę bibliotekę do naszego projektu i zaimportować ją jak poniżej:

```Clojure
[net.cgrand/enlive "1.1.6"]
(require '[net.cgrand.enlive-html :as html])
```

Następnie, musimy użyć funkcji `html/parse` w celu sparsowania strony internetowej. Przykładowo, chcemy sparsować tytuł artykułu z witryny Clojure.org:

```Clojure
(def parsed-html (html/parse "https://clojure.org/"))
```

Teraz, aby uzyskać tytuł artykułu, musimy zastosować selektor CSS:

```Clojure
(html/select parsed-html ["h2" {:id "clojurevsjava-intro"}])
```

Kod powyżej użyje selektora CSS `h2` z atrybutem `id` o wartości `clojurevsjava-intro` do znalezienia i sparsowania tytułu artykułu z witryny Clojure.org. Wynik powinien wyglądać następująco:

```Clojure
[{:tag :h2
  :attrs nil
  :content
  ("Clojure vs. Java: Married... with Children?")}]
```

## Głębsze zagłębienie
Enlive jest potężną biblioteką, która może nam pomóc w parsowaniu HTML w bardziej zaawansowany sposób, np. wybierając elementy po klasie lub identyfikatorze, szukając po wyrażeniach regularnych i wiele innych. Więcej informacji na temat Enlive można znaleźć w jego dokumentacji.

## Zobacz także
- Dokumentacja Enlive: https://github.com/cgrand/enlive
- Dokumentacja Clojure: https://clojure.org/
- Squirrel SQL Client: https://www.squirrelsql.org/