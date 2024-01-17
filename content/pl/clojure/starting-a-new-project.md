---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Clojure: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego? 
Rozpoczęcie nowego projektu jest po prostu procesem tworzenia czegoś nowego w języku programowania Clojure. Programiści robią to, aby wdrożyć nowe funkcjonalności, naprawić błędy lub ulepszyć istniejący kod.

## Jak to zrobić:
### Tworzenie nowego projektu:
```Clojure
lein new app <nazwa_projektu>
```
Sample output:
Tworzenie nowego projektu lein_new_app
### Kompilacja projektu:
```Clojure
lein uberjar
```
Sample output:
Kompilacja projektu lein_uberjar_app

## Głębszy przegląd:
### Kontekst historyczny:
Clojure został opracowany w 2007 roku przez Richa Hickeya, aby zapewnić programistom funkcjonalność języka Lisp na platformie JVM. Jest to język programowania funkcyjnego, znany z wydajnego przetwarzania danych i skalowalności.

### Alternatywy:
Inną alternatywą dla Clojure jest język Java, który jest również zaprojektowany dla platformy JVM. Jednak Clojure oferuje prostszą składnię i większą wydajność przy przetwarzaniu danych.

### Szczegóły implementacji:
Clojure korzysta z maszyny wirtualnej Javy (JVM) i może korzystać z bibliotek Javy. Jest to również język częściowo funkcjonalny i częściowo obiektowy.

## Zobacz też:
https://clojure.org/ - Oficjalna strona języka Clojure
https://github.com/technomancy/leiningen - Dokumentacja Leiningena, narzędzia do budowania i zarządzania projektami Clojure
https://www.braveclojure.com/clojure-for-the-brave-and-true/ - Darmowa książka o nauce języka Clojure dla początkujących.