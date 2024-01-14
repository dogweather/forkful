---
title:                "Haskell: Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy usuwać niechciane znaki w tekście, które pasują do pewnego wzorca. Może to być ważne, gdy tworzymy aplikację, która wymaga czystego i dokładnego tekstu, takiego jak przetwarzanie danych czy generowanie raportów. W takich przypadkach usuwanie znaków musi być dokładne i efektywne.

## Jak to zrobić

Język Haskell zapewnia prosty i wydajny sposób na usuwanie znaków z tekstu. Wystarczy skorzystać z funkcji `filter`, która pozwala na filtrowanie listy elementów według określonego warunku. W naszym przypadku warunkiem będzie funkcja `notElem`, która sprawdza czy dany znak nie należy do podanego zbioru. W ten sposób możemy wybrać tylko te znaki, które nie pasują do naszego wzorca i usunąć je z tekstu. Niech przykład poniżej pokaże, jak łatwo i szybko można to zrobić:

```Haskell
string = "To jest przykładowy tekst z niechcianymi znakami #$%^&*"
pattern = "#$%^&*"

cleanedString = filter (`notElem` pattern) string

-- Output:
-- "To jest przykładowy tekst z niechcianymi znakami "
```

## Dogłębna analiza

Funkcja `filter` i `notElem` wykorzystują wbudowaną w Haskell "lista składana". Polega ona na tworzeniu listy wynikowej poprzez przetwarzanie każdego elementu z listy początkowej za pomocą odpowiedniej funkcji. W przypadku `filter`, funkcja ta zwraca `True`, jeśli element nie spełnia określonego warunku, co wyklucza go z listy wynikowej. W ten sposób możemy usunąć wszystkie niechciane znaki z tekstu przez przejrzenie i wybranie tylko tych, które nie pasują do naszego wzorca.

## Zobacz też

1. Dokumentacja Haskell - https://www.haskell.org/documentation/
2. Tutoriale Haskell - https://github.com/turboMaCk/awesome-haskell
3. Wprowadzenie do funkcyjnego programowania - https://pl.wikibooks.org/wiki/Haskell/Wprowadzenie_do_funkcyjnego_programowania