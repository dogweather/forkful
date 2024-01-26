---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Co to jest Wielka Litera i po co to robimy? Capitalizing a string oznacza zmianę pierwszych liter słów na wielkie, co przydaje się przy formatowaniu tekstów jak tytuły czy nagłówki. Programiści używają tego do uporządkowania danych i poprawienia czytelności.

## How to:
Tu masz przykłady jak to zrobić:

```PHP
<?php
// Pierwsza litera na wielką.
$greeting = 'witaj świecie!';
$capitalizedGreeting = ucfirst($greeting);
echo $capitalizedGreeting; // Wyjdzie: "Witaj świecie!"

// Wszystkie słowa na wielką literę.
$title = 'przykład kapitalizacji tytułu';
$capitalizedTitle = ucwords($title);
echo $capitalizedTitle; // Wyjdzie: "Przykład Kapitalizacji Tytułu"
?>
```

## Deep Dive
Historia: Pierwsze funkcje do kapitalizacji w PHP pojawiły się lata temu i stały się standardem w manipulacji tekstem.

Alternatywy: Oprócz `ucfirst()` i `ucwords()`, możesz użyć `strtoupper()` żeby wszystko było WIELKIMI literami lub `mb_convert_case()` dla wielojęzycznej obsługi.

Implementacja: `ucfirst()` zmienia tylko pierwszą literę. `ucwords()` idzie przez cały string i szuka spacji, żeby zmieniać następujące po nich litery na wielkie. Uważaj na języki z multibajtowymi znakami – standardowe funkcje mogą nie zadziałać poprawnie.

## See Also:
- Oficjalna dokumentacja PHP na ucwords: https://www.php.net/manual/en/function.ucwords.php
- Oficjalna dokumentacja PHP na ucfirst: https://www.php.net/manual/en/function.ucfirst.php
- Multibajtowa alternatywa, mb_convert_case: https://www.php.net/manual/en/function.mb-convert-case.php
