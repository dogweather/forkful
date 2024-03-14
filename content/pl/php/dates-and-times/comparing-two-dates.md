---
date: 2024-01-20 17:33:38.708295-07:00
description: "Por\xF3wnywanie dw\xF3ch dat to sprawdzanie, kt\xF3ra z nich jest wcze\u015B\
  niejsza, p\xF3\u017Aniejsza czy mo\u017Ce identyczna. Programi\u015Bci robi\u0105\
  \ to, by obs\u0142ugiwa\u0107 logik\u0119 zwi\u0105zan\u0105\u2026"
lastmod: '2024-03-13T22:44:35.509901-06:00'
model: gpt-4-1106-preview
summary: "Por\xF3wnywanie dw\xF3ch dat to sprawdzanie, kt\xF3ra z nich jest wcze\u015B\
  niejsza, p\xF3\u017Aniejsza czy mo\u017Ce identyczna. Programi\u015Bci robi\u0105\
  \ to, by obs\u0142ugiwa\u0107 logik\u0119 zwi\u0105zan\u0105\u2026"
title: "Por\xF3wnywanie dw\xF3ch dat"
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
