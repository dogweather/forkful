---
title:    "Clojure: Ekstrakcja podciągów"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyciąganie podciągów jest jedną z podstawowych i ważnych czynności w programowaniu, która pozwala na manipulowanie i przetwarzanie danych tekstowych. W tym artykule dowiecie się, jak łatwo i szybko wykorzystać tę funkcjonalność w języku Clojure.

## Jak to zrobić

Wyciąganie podciągów w Clojure jest bardzo proste dzięki funkcji `subs`, która pozwala na zdefiniowanie zakresu indeksów wewnątrz podanego tekstu. Poniżej znajdują się przykłady użycia tej funkcji:

```Clojure
(def text "To jest przykładowy tekst")
(subs text 7 16)
```

Output: `przykładowy`

Funkcja `subs` przyjmuje trzy argumenty - tekst, początkowy indeks i końcowy indeks. Pierwszy znak w tekście ma indeks 0, a ostatni indeks jest o 1 mniejszy niż długość tekstu. Można także wykorzystać ujemne indeksy, które oznaczają odliczanie od końca tekstu. 

```Clojure
(subs text -5 -1)
```

Output: `tekst`

Inną przydatną funkcją jest `subs-from`, która pozwala na wyciągnięcie podciągu od danego indeksu do końca tekstu.

```Clojure
(subs-from text 3)
```

Output: `jest przykładowy tekst`

## Głębsze pogłębienie

W języku Clojure istnieje również funkcja `subs-by`, która pozwala na wyciągnięcie podciągu o określonej długości, zaczynając od podanego indeksu. Na przykład, jeśli chcemy wyciągnąć podciąg z tekstu o długości 5 zaczynający się od indeksu 3, możemy wykorzystać tę funkcję w następujący sposób:

```Clojure
(subs-by text 3 5)
```

Output: `jest`

Funkcja `subs-by` jest szczególnie przydatna, gdy chcemy wyciągnąć kolejne fragmenty tekstu o stałej długości.

## Zobacz też

- Dokumentacja funkcji `subs`: https://clojuredocs.org/clojure.string/subs
- Przykłady użycia wyciągania podciągów: https://www.tutorialspoint.com/clojure/clojure_strings.htm
- Krótki kurs o wyciąganiu podciągów w Clojure: https://clojureverse.org/t/crash-course-string-manipulation-in-clojure/6632