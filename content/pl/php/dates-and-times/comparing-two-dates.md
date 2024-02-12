---
title:                "Porównywanie dwóch dat"
aliases: - /pl/php/comparing-two-dates.md
date:                  2024-01-20T17:33:38.708295-07:00
model:                 gpt-4-1106-preview
simple_title:         "Porównywanie dwóch dat"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Porównywanie dwóch dat to sprawdzanie, która z nich jest wcześniejsza, późniejsza czy może identyczna. Programiści robią to, by obsługiwać logikę związaną z terminami, wydarzeniami, i wszelkiego rodzaju czasowymi warunkami w aplikacjach.

## Jak to zrobić:

Porównajmy dwie daty w PHP z wykorzystaniem obiektu `DateTime`:

```PHP
<?php
$date1 = new DateTime("2021-03-15");
$date2 = new DateTime("2021-03-20");

if ($date1 < $date2) {
    echo "Data 1 jest wcześniejsza niż Data 2";
} elseif ($date1 > $date2) {
    echo "Data 1 jest późniejsza niż Data 2";
} else {
    echo "Data 1 jest taka sama jak Data 2";
}
?>
```

Wynik działania:
```
Data 1 jest wcześniejsza niż Data 2
```

## Deep Dive

Porównywanie dat w PHP stało się proste, gdy wprowadzono obiekty `DateTime` w wersji 5.2.0. Zastąpiły one starsze funkcje, jak `strtotime` czy array `getdate`, które nadal są w użyciu, ale w nowych projektach preferowane są obiekty. `DateTime` daje nie tylko czytelniejszy kod, ale też większą precyzję i elastyczność.

Alternatywy to m.in. `strtotime`, który przekształca tekstowe reprezentacje daty na Unix timestamp, oraz operatory bezpośredniego porównania dla timestampów.

Przykład z `strtotime`:
```PHP
<?php
$timestamp1 = strtotime("2021-03-15");
$timestamp2 = strtotime("2021-03-20");

if ($timestamp1 < $timestamp2) {
    echo "Data 1 jest wcześniejsza niż Data 2";
}
?>
```

Przy implementacji warto pamiętać o strefach czasowych - jeśli pracujemy na globalnych aplikacjach, różnice w czasie mogą mieć znaczenie. Do `DateTime` można przekazać strefę czasową jako drugi argument konstruktora.

## Zobacz też:

- [Dokumentacja klasy DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Poradnik jak używać DateTime w PHP](https://www.php.net/manual/en/datetime.construct.php)
- [Funkcja strtotime](https://www.php.net/manual/en/function.strtotime.php)
