---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Kotlin: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Czego i dlaczego?

Konwertowanie ciągu znaków na małe litery jest częstym zadaniem w programowaniu. Polega na zamianie wszystkich znaków w ciągu na ich odpowiedniki w małych literach. Robimy to, aby ułatwić sobie pracę z danymi i uniknąć problemów wynikających z błędów typograficznych.

## Jak to zrobić:

Do konwertowania ciągu znaków na małe litery w języku Kotlin używamy metody `toLowerCase()`. Przykładowa implementacja może wyglądać tak:

```Kotlin
val text = "PRZYKŁADOWY CIĄG ZNAKÓW"
val convertedText = text.toLowerCase()
println(convertedText)
```

W powyższym przykładzie wykorzystujemy zmienną `text`, która zawiera ciąg znaków w wielkich literach. Następnie, dzięki wywołaniu metody `toLowerCase()`, przypisujemy konwertowany ciąg do zmiennej `convertedText`. W rezultacie, po wywołaniu metody `println()`, otrzymujemy w konsoli konwertowany ciąg znaków z małymi literami.

## Głębsze spojrzenie:

Konwertowanie ciągu znaków na małe litery jest popularnym zadaniem zarówno w języku Kotlin, jak i w innych językach programowania. Przez lata wykształciło się wiele różnych technik i metod tego procesu, jednak najbardziej powszechną i efektywną jest metoda `toLowerCase()`. Alternatywne sposoby takie jak użycie funkcji `toUpperCase()` lub zamiana pojedynczych znaków przy pomocy pętli for, mogą być mniej wydajne i skuteczne.

Implementacja metody `toLowerCase()` w języku Kotlin opiera się na standardowej funkcji dostępnej w języku Java, która przekształca wszystkie litery na małe za pomocą odpowiedniego algorytmu. Jest to także ułatwienie dla programistów, którzy muszą pracować z danymi pobranymi z różnych źródeł i w różnych formatach.

## Zobacz także:

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/
- Porównanie metody `toLowerCase()` z innymi sposobami konwersji: https://medium.com/@olegnikitinworking/comparing-tostring-method-in-java-and-kotlin-de59a7871376
- Przykładowe użycie metody `toLowerCase()` w praktyce: https://blog.ippon.tech/converting-string-to-lower-case-in-kotlin/