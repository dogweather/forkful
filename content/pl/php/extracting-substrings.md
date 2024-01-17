---
title:                "Wyciąganie podciągów"
html_title:           "PHP: Wyciąganie podciągów"
simple_title:         "Wyciąganie podciągów"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wyciąganie podciągów (lub części) jest jedną z podstawowych operacji w programowaniu. Polega ona na pobraniu wybranego fragmentu tekstu z ciągu znaków. Programiści często używają tej techniki do przetwarzania i analizowania danych tekstowych, na przykład podczas tworzenia systemów wyszukiwania lub filtrowania danych.

## Jak to zrobić:

W PHP istnieje kilka wbudowanych funkcji do wyciągania podciągów, w tym `substr ()`, `mb_substr ()` i `explode ()`. Oto przykładowy kod, który wykorzystuje funkcję `substr ()` do pobrania części tekstu:

```PHP
$text = "Witaj na mojej stronie internetowej!";
$substring = substr($text, 6, 9);

echo $substring; // wynik: "na mojej"
```

W powyższym przykładzie, funkcja `substr ()` pobiera tekst od szóstego znaku (licząc od zera) i zwraca długość dziewięciu znaków. Można również użyć funkcji `mb_substr ()` do wyciągania znaków z uwzględnieniem wielokrotności bajtów, co jest przydatne podczas obsługi danych wielojęzycznych. Aby rozdzielić ciąg znaków na części, można użyć `explode ()`, co zwróci tablicę z podanymi ciągami jako elementami.

## Głębokie Nurkowanie:

Wyciąganie podciągów jest powszechną techniką, która istnieje od dawna w programowaniu. Wcześniej programiści często musieli tworzyć własne funkcje do tego celu, jednak obecnie większość języków programowania ma wbudowane funkcje do wyciągania podciągów. Alternatywnie, można również użyć wyrażeń regularnych do bardziej skomplikowanych przypadków, takich jak wyszukiwanie określonych wzorców w tekście.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej na temat wyciągania podciągów w PHP, poniżej znajdują się przydatne linki:

- [Dokumentacja PHP o funkcji substr ()](https://www.php.net/manual/en/function.substr.php)
- [Wskazówki dotyczące wykorzystywania wyrażeń regularnych w PHP](https://www.php.net/manual/en/regexp.reference.php)