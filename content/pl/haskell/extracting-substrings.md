---
title:                "Ekstrakcja podciągów"
html_title:           "Haskell: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

##Dlaczego

Jedną z podstawowych operacji na stringach jest wydobywanie podciągów. Jest to przydatne w wielu sytuacjach, na przykład gdy chcemy wyodrębnić tylko część tekstowej informacji lub porównywać podciągi ze sobą. W tym artykule dowiesz się jak w prosty sposób wydobywać podciągi w języku Haskell.

##Jak To Zrobić

W pierwszej kolejności musimy zaimportować moduł `Data.List`, który zawiera funkcje do operacji na listach. Następnie możemy użyć funkcji `take` oraz `drop` do wydobycia podciągu. Weźmy za przykład następujący string:

```Haskell
let tekst = "Witaj w świecie Haskell!"
```

Aby wydobyć pierwszych pięć znaków tego tekstu, możemy użyć funkcji `take` w następujący sposób:

```Haskell
take 5 tekst
```

Co zwróci nam podciąg "Witaj". Natomiast, jeśli chcemy wydobyć podciąg zaczynający się od szóstego znaku, możemy skorzystać z funkcji `drop`:

```Haskell
drop 6 tekst
```

Co zwróci nam podciąg "w świecie Haskell!".

Jeśli potrzebujemy wydobyc podciąg o dowolnej długości, możemy użyć funkcji `take` i `drop` w połączeniu z funkcją `length` do obliczenia długości tekstu. Na przykład, jeśli chcemy wydobyć podciąg zawierający ostatnie 10 znaków tekstu, możemy użyć następującego kodu:

```Haskell
take 10 (drop ((length tekst)-10) tekst)
```

Co zwróci nam podciąg "Haskell!".

##Deep Dive

Funkcje `take` i `drop` są bardzo proste w użyciu, ale warto wiedzieć, że będą działać tylko wtedy gdy przekażemy im poprawną liczbę elementów. Jeśli przekazana liczba będzie większa niż długość tekstu, funkcja `take` zwróci cały tekst, a funkcja `drop` pusty string. Natomiast, jeśli przekażemy liczbę ujemną, funkcja `take` zwróci pusty string, a funkcja `drop` cały tekst. Dlatego ważne jest aby zadbać o przekazanie poprawnych danych do funkcji.

##Zobacz także

Jeśli chcesz dowiedzieć się więcej o operacjach na stringach w Haskellu, możesz zapoznać się z poniższymi artykułami:

- [Operacje na stringach w Haskellu](https://wiki.haskell.org/Strings)
- [Funkcyjna manipulacja stringami w Haskellu](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/functional-manipulation-of-strings)
- [Wydobywanie tekstu przy użyciu wyrażeń regularnych w Haskellu](https://stackoverflow.com/questions/13374066/extract-string-with-regular-expression-in-haskell)