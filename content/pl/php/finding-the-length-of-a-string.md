---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "PHP: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Mając do wykonania pewne zadanie związane z manipulacją tekstem w PHP, często potrzebujemy dowiedzieć się, ile znaków składa się na dany string. Poznanie długości stringa jest więc kluczowym krokiem w wielu projektach, dlatego też warto poznać różne sposoby na wykonywanie tego zadania.

## Jak To Zrobić
```PHP
// Przykładowy string
$string = "Programowanie PHP jest super!";

// Wykorzystanie wbudowanej funkcji "strlen()"
echo strlen($string); 
// output: 28

// Użycie metody "mb_strlen()" do uwzględnienia wielkości znaków UTF-8
echo mb_strlen($string);
// output: 28
```

## Deep Dive
W PHP mamy dostępne dwie podstawowe metody do liczenia długości stringa: `strlen()` i `mb_strlen()`. Pierwsza z nich jest wbudowana w język i zwraca liczbę bajtów, które zajmuje dany string. Natomiast `mb_strlen()` jest funkcją z biblioteki mbstring, która uwzględnia wielkość znaków unicode, dzięki czemu lepiej radzi sobie z językami będącymi poza standardowym alfabetem.

Warto również pamiętać, że w PHP istnieje również funkcja `iconv_strlen()`, która umożliwia liczenie długości stringa w danym zestawie znaków. Może to być przydatne, gdy potrzebujemy poznać długość tekstu w formacie innym niż Unicode.

## Zobacz Również
- [Oficjalna dokumentacja PHP dla funkcji strlen()](https://www.php.net/manual/en/function.strlen.php)
- [Oficjalna dokumentacja PHP dla funkcji mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [Oficjalna dokumentacja PHP dla funkcji iconv_strlen()](https://www.php.net/manual/en/function.iconv-strlen.php)