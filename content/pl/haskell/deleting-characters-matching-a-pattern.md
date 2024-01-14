---
title:    "Haskell: Usuwanie znaków pasujących do wzorca"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami, gdy piszesz kod w Haskellu, możesz natknąć się na sytuację, w której chcesz usunąć pewne znaki pasujące do wzoru. Może to być wymagane, aby przetworzyć dane lub poprawić błędy w tekście. W tej krótkiej instrukcji dowiecie się, jak to zrobić.

## Jak to zrobić

Aby usunąć znaki pasujące do wzoru w Haskellu, można użyć funkcji `filter`, która przyjmuje predykat i listę, a zwraca listę zawierającą tylko elementy spełniające ten predykat. Na przykład, jeśli chcemy usunąć wszystkie samogłoski z tekstu, możemy użyć poniższego kodu:

```Haskell
tekst = "Witaj w Haskellu"
filtr = filter (`notElem` "aeiouy")
tekstBezSamoglosek = filtr tekst
putStrLn tekstBezSamoglosek
```

Output:
"Wtj w Hskll"

Tutaj, `notElem` jest funkcją predefiniowaną w Haskellu, która sprawdza, czy dany element nie znajduje się w danej liście. Możemy więc przekazać ją jako argument do funkcji `filter` i użyć jej do wyodrębnienia wszystkich znaków, które nie są samogłoskami.

Jest to tylko jeden z wielu sposobów na usunięcie znaków pasujących do wzoru w Haskellu. Istnieje wiele innych funkcji, takich jak `map` i lambda wyrażenia, które również mogą być użyte w tym celu. Warto więc spróbować różnych opcji i wybrać tę, która najlepiej odpowiada danemu problemowi.

## Deep Dive

Jeśli chcesz poznać więcej szczegółów na temat usuwania znaków pasujących do wzoru w Haskellu, warto zwrócić uwagę na funkcję `dropWhile`, która działa bardzo podobnie do `filter`. Jedyna różnica polega na tym, że `dropWhile` usuwa wszystkie elementy spełniające warunek, dopóki nie napotka pierwszego, który go nie spełnia. Następnie zwraca już tylko resztę listy.

Możemy wykorzystać tę funkcję w połączeniu z funkcją `isSpace` z modułu `Data.Char`, aby usunąć wszystkie spacje z tekstu. Przykładowy kod może wyglądać następująco:

```Haskell
tekst = "Witaj   w    Haskellu"
dropSpace = dropWhile (isSpace)
tekstBezSpacji = dropSpace tekst
putStrLn tekstBezSpacji
```

Output:
"Witaj w Haskellu"

`isSpace` jest funkcją, która sprawdza, czy dany znak jest białym znakiem (np. spacja, tabulacja). Możemy więc przekazać ją jako argument do funkcji `dropWhile` i wykorzystać do usunięcia wszystkich spacji z tekstu.

W ten sposób możemy uzyskać bardziej precyzyjną kontrolę nad usuwaniem znaków pasujących do wzoru i dopasować go do konkretnych potrzeb.

## Zobacz również

- [Dokumentacja funkcji `filter` w Haskellu](https://www.haskell.org/onlinereport/standard-prelude.html#filter)
- [Dokumentacja modułu `Data.Char`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:isSpace)
- [Tutorial o manipulacji stringami w Haskellu](https://riptutorial.com/haskell/topic/5/manipulating-strings)