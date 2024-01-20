---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Porównywanie dwóch dat to proces ustalania, która data jest wcześniejsza, późniejsza lub czy obie są identyczne. Programiści robią to, aby kontrolować sekwencję wydarzeń lub mierzyć różnice między datami.

## Jak to zrobić:

Porównanie dat w PHP jest dość proste. Możemy to zrobić za pomocą wbudowanej funkcji `strtotime()` i operatorów porównania. Sprawdź przykładowy kod poniżej:

```PHP
<?php
  $date1 = "2022-04-05";
  $date2 = "2022-08-10";

  if (strtotime($date1) < strtotime($date2)) {
    echo "$date1 jest wcześniejszy niż $date2";
  } else if (strtotime($date1) > strtotime($date2)) {
    echo "$date1 jest późniejszy niż $date2";
  } else {
    echo "Obie daty są identyczne";
  }
?>
```

Gdy uruchomisz ten kod, otrzymasz wyniki takie jak:

```
2022-04-05 jest wcześniejszy niż 2022-08-10
```

## Deep Dive

Historia: PHP zawsze oferował proste i intuicyjne metody do porównywania dat za pomocą funkcji `strtotime()`. Niezależnie od wersji PHP, ta metoda pozostaje efektywna.

Alternatywy: Oprócz `strtotime()`, możemy użyć obiektowego podejścia z obiektami `DateTime`. Pozwoli to na wyrafinowanej kontrolę i różnorodne operacje na datach.

Implementacja: Funkcja `strtotime()` konwertuje ciąg znaków na timestamp Unix, który jest liczbą sekund od 1 stycznia 1970 roku, co następnie umożliwia prostą operację porównania.

```PHP
<?php
  $date1 = new DateTime("2022-04-05");
  $date2 = new DateTime("2022-08-10");

  if ($date1 < $date2) {
    echo $date1->format('Y-m-d') . " jest wcześniejszy niż " . $date2->format('Y-m-d');
  } else if ($date1 > $date2) {
    echo $date1->format('Y-m-d') . " jest późniejszy niż " . $date2->format('Y-m-d');
  } else {
    echo "Obie daty są takie same";
  }
?>
```

## Zobacz też

- PHP Manual: Funkcje daty i czasu: https://www.php.net/manual/pl/book.datetime.php
- PHP Manual: DateTime::diff: https://www.php.net/manual/pl/datetime.diff.php

Pamiętaj, że najlepsze rozwiązanie zależy od specyfiki Twojego projektu i wymagań biznesowych. Experimentuj z różnymi podejściami i wybierz najlepsze dla ciebie!