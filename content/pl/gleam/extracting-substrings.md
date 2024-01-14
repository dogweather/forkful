---
title:    "Gleam: Wydobywanie podciągów"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym wpisie opowiemy o tym, jak wykorzystać funkcję w języku Gleam do ekstrakcji podciągów. Poznamy również dlaczego jest to przydatne narzędzie w programowaniu.

## Jak To Zrobić

Ekstrakcja podciągów to proces wyodrębniania części tekstu z zadanego ciągu znaków. W języku Gleam, możemy tego dokonać za pomocą funkcji ```extract_substring```  która przyjmuje jako argumenty dwa indeksy określające pozycje początkową i końcową podciągu.

Przykładowo, chcąc wyodrębnić podciąg "program" ze zdania "Lubię pisać programy w języku Gleam." użylibyśmy następującego kodu:

```Gleam
let zdanie = "Lubię pisać programy w języku Gleam."
let podciag = extract_substring(zdanie, 10, 16)
```

Po wykonaniu tej funkcji, wartość zmiennej ```podciag``` zostanie ustawiona na "program", co można zweryfikować za pomocą funkcji ```show()```:

```Gleam
show(podciag)
```

Powyższy kod wyświetli "program" w konsoli. Możemy również wyświetlić zadany podciąg wraz z pozostałymi fragmentami zdania, używając operatora konkatenacji (+) :

```Gleam
show("Język " + podciag + "ingi w " + zdanie)
```

Co wyświetli "Lubię pisać programingi w języku Gleam." w konsoli.

## Deep Dive

Funkcja ```extract_substring``` jest bardzo przydatna przy manipulacji tekstem w języku Gleam. Możemy wykorzystać ją na wiele sposobów, np. do filtrowania lub modyfikowania danych tekstowych.

Ponadto, możemy także określić ujemne wartości indeksów, co pozwala na wyodrębnienie podciągów od końca ciągu znaków. Przykładowo, użycie indeksów -5 i -1 w wyżej podanym kodzie spowoduje wyodrębnienie podciągu "Gleam" z oryginalnego zdania.

## Zobacz też

1. Dokumentacja języka Gleam: https://gleam.run/
2. Wideo-tutorial o ekstrakcji podciągów w języku Gleam: https://www.youtube.com/watch?v=09uRDfL1NK4
3. Przykładowe zadania wykorzystujące funkcję ```extract_substring```: https://github.com/gleam-lang/gleam/issues/1298