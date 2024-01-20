---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie bieżącej daty oznacza uzyskiwanie informacji o aktualnym dniu, miesiącu i roku. Programiści robią to, aby monitorować logi, napisać funkcje zależne od czasu, lub do znaczników czasu.

## Jak to zrobić:

Aby uzyskać bieżącą datę w PHP, korzystamy z funkcji `date()`. Oto kod:

```PHP 
<?php
echo date('Y-m-d');
?>
```

Wynik dzisiejszego dnia wygląda tak:

```PHP
2022-01-31
```

Możesz również dostać czas razem z datą:

```PHP
<?php
echo date('Y-m-d H:i:s');
?>
```

Wynik wygląda tak:

```PHP
2022-01-31 14:52:06
```

## Pogłębiona analiza

Historia funkcji `date()` w PHP sięga początków języka. Działa na podstawie tzw. timestamp, który jest przedstawiany jako liczba sekund, które upłynęły od "ery Unix" czyli 1 stycznia 1970 roku. Alternatywą dla funkcji date() jest funkcja `DateTime()`.

Funkcja `DateTime()` jest bardziej złożona, ale pozwala na więcej operacji na dacie i czasie:

```PHP
<?php
$now = new DateTime();
echo $now->format('Y-m-d H:i:s');
?>
```

Wynik jest taki sam jak w przypadku funkcji `date()`:

```PHP
2022-01-31 14:52:06
```

## Zobacz także

- Dokumentacja PHP na temat funkcji [date()](http://php.net/manual/en/function.date.php) (EN).
- Dokumentacja PHP na temat klasy [DateTime()](http://php.net/manual/en/class.datetime.php) (EN).
- Artykuł na [PHPenthusiast](https://phpenthusiast.com/blog/working-with-date-and-time-in-php) na temat pracy z datami i czasem w PHP (EN).