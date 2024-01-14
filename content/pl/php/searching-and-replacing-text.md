---
title:                "PHP: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w trakcie tworzenia aplikacji internetowych musimy dokonać zmian w tekście. Mogą to być np. poprawki ortograficzne lub zmiana nazw zmiennych w kodzie. W takich przypadkach bardzo przydatną i często używaną funkcją jest wyszukiwanie i zamiana tekstu. W tym artykule opiszemy, jak wykorzystać tę funkcję w języku PHP.

## Jak To Zrobić

Do wyszukiwania i zamiany tekstu w PHP możemy wykorzystać funkcję `str_replace()`. Jej składnia wygląda następująco:

```PHP
str_replace($co_wyszukujemy, $na_co_zamieniamy, $tekst);
```

W miejscu `$co_wyszukujemy` wpisujemy szukaną frazę, a w miejscu `$na_co_zamieniamy` wpisujemy tekst, na który chcemy ją zamienić. Ostatnim argumentem jest sam tekst, w którym dokonujemy zmian.

Przykład:

```PHP
$tekst = "Jestem programistą PHPP.";

$nowy_tekst = str_replace("PHPP", "PHP", $tekst);

echo $nowy_tekst;
```

W powyższym przykładzie wynikiem będzie napis "Jestem programistą PHP.".

Innym sposobem jest użycie funkcji `preg_replace()`, która wykorzystuje wyrażenia regularne do wyszukania i zamiany tekstu.

Przykład:

```PHP
$tekst = "Wpisz swoje dane: imię i nazwisko.";

$nowy_tekst = preg_replace("/imię i nazwisko/", "imię i nazwisko: John Smith", $tekst);

echo $nowy_tekst;
```

W tym przypadku wynikiem będzie "Wpisz swoje dane: imię i nazwisko: John Smith.".

## Deep Dive

W PHP możemy także dokonać wielokrotnej zamiany tekstu za pomocą funkcji `strtr()`. Jej składnia wygląda następująco:

```PHP
strtr($tekst, $tablica);
```

Jako drugi argument musimy podać tablicę, w której kluczami są szukane frazy, a wartościami są teksty, na które chcemy je zamienić.

Przykład:

```PHP
$tekst = "Jestem programistą PHPP.";

$tablica = array("PHPP" => "PHP", "programistą" => "programistą języka");

$nowy_tekst = strtr($tekst, $tablica);

echo $nowy_tekst;
```

Wynikiem będzie napis "Jestem programistą języka PHP.".

## Zobacz także

- Dokumentacja funkcji `str_replace()` w języku PHP: https://www.php.net/manual/en/function.str-replace.php
- Przykłady użycia funkcji `preg_replace()` w języku PHP: https://www.geeksforgeeks.org/php-preg-replace-function/
- Szczegóły na temat funkcji `strtr()` w języku PHP: https://www.php.net/manual/en/function.strtr.php