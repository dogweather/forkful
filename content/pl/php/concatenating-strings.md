---
title:                "PHP: Konkatenacja ciągów znaków"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ciągła konkatenacja ciągów znaków jest niezbędna w programowaniu PHP, ponieważ pozwala nam na łączenie różnych informacji w jedno, spójne wyrażenie. Dzięki temu możemy wyświetlać użytkownikom bardziej złożone i czytelne komunikaty, a także budować dynamiczne zapytania do bazy danych.

## Jak to zrobić

Do konkatenacji ciągów znaków w PHP wykorzystujemy operator kropki (".") oraz znaki cudzysłowia (" "), aby oddzielać poszczególne wyrażenia. Przykładowy kod wyglądałby następująco:

```PHP
$imie = "Jan";
$nazwisko = "Kowalski";

echo "Witaj, " . $imie . " " . $nazwisko . "!"; 
// wyświetli "Witaj, Jan Kowalski!"
```

Możemy również łączyć więcej niż dwie zmienne w jednym wyrażeniu, korzystając z operatora kropki. Pamiętajmy, że w przypadku konkatenacji liczb, PHP automatycznie przekonwertuje je na ciąg znaków.

## Głębszy zanurzenie

Podczas konkatenacji ciągów znaków w PHP możemy również wykorzystać funkcję `implode()`, która pozwala nam na łączenie tablic w jeden string. Przykładowy kod wykorzystujący tę funkcję wyglądałby w ten sposób:

```PHP
$imiona = array("Jan", "Kasia", "Tomek");

echo "Nasi goście to: " . implode(", ", $imiona) . "."; 
// wyświetli "Nasi goście to: Jan, Kasia, Tomek."
```

Miejmy również na uwadze, że operator kropki może być wykorzystany również w innych kontekstach, na przykład do łączenia wartości w zapytaniach SQL.

## Zobacz również

- Dokumentacja PHP na temat konkatenacji: https://www.php.net/manual/en/language.operators.string.php
- Przykładowe kody konkatenacji w PHP: https://www.w3schools.com/php/php_string_concatenation.asp
- Wyjaśnienie funkcji implode(): https://www.php.net/manual/en/function.implode.php