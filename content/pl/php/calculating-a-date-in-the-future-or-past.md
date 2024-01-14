---
title:    "PHP: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami w tworzeniu aplikacji internetowych musimy wyświetlić datę w przyszłości lub w przeszłości. Może to być na przykład przewidywana data dostawy zamówienia lub data urodzenia użytkownika. W tym artykule dowiesz się, jak w prosty sposób obliczyć taką datę w języku PHP.

## Jak to zrobić

Często kodowanie dat w PHP może być mylące, szczególnie jeśli nie korzystamy z gotowych bibliotek. Dlatego przygotowaliśmy dla Ciebie kilka prostych przykładów, które korzystają z wbudowanych funkcji PHP dotyczących dat. Zobaczmy, jak w prosty sposób obliczyć datę w przyszłości lub w przeszłości.

```PHP
<?php
// Obecna data
$today = date("d-m-Y");

// Obliczanie jutra
$tomorrow = date('d-m-Y', strtotime("+1 day"));

// Obliczanie daty za 2 tygodnie
$twoWeeks = date('d-m-Y', strtotime("+2 weeks"));

echo "Dzisiaj jest $today <br>";
echo "Jutro będzie $tomorrow <br>";
echo "Za dwa tygodnie będzie $twoWeeks";
?>
```

Wynikiem powyższego kodu będzie:

```
Dzisiaj jest 02-07-2021
Jutro będzie 03-07-2021
Za dwa tygodnie będzie 16-07-2021
```

Możemy również obliczyć datę w przyszłości lub w przeszłości, wykorzystując podstawowe działania matematyczne. Na przykład, jeśli chcemy obliczyć datę za 5 dni, możemy zastosować następujący kod:

```PHP
<?php
$today = date("d-m-Y");
$fiveDays = date('d-m-Y', strtotime($today. ' + 5 days'));

echo "Dzisiaj jest $today <br>";
echo "Za pięć dni będzie $fiveDays";
?>
```

## Zagłębienie się

W języku PHP istnieje wiele innych funkcji i metod do operowania na datach, takich jak `mktime()`, `DateTime` czy `DateInterval`. Warto poznać je, aby móc lepiej korzystać z możliwości wyliczania dat w przyszłości lub w przeszłości. Pamiętaj również, że daty mogą być wyświetlane w różnych formatach, więc warto zwrócić uwagę na to, jakie formatowanie jest wymagane przez Twój projekt.

## Zobacz także

- [Dokumentacja PHP: Data i godzina](https://www.php.net/manual/pl/datetime.html)
- [PHP.net: Sugestie dla pracy z datami](https://www.php.net/manual/pl/book.datetime.php)
- [PHP Date and Time Functions - W3schools](https://www.w3schools.com/php/php_ref_date.asp)