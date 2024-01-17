---
title:                "Znalezienie długości ciągu znaków"
html_title:           "PHP: Znalezienie długości ciągu znaków"
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Poznawanie długości łańcucha to podstawowa czynność, która pozwala programistom na manipulację tekstem. Jest to dokładne określenie liczby znaków w ciągu znaków, co jest niezbędne do wielu operacji, takich jak walidacja danych, generowanie kodów lub obliczanie wpływu na wydajność aplikacji.

## Jak to zrobić:
### Przykłady kodu:
Podstawowe podejście do znalezienia długości łańcucha w PHP wykorzystuje funkcję wbudowaną `strlen()`. Poniżej znajduje się przykład wykorzystania tej funkcji w programie zwracającym długość łańcucha "Hello World!":

```
<?php
$hello = "Hello World!";
echo strlen($hello);
?>
```

> Output: 12

W celu zabezpieczenia przed możliwością wprowadzenia przez użytkownika niepożądanych znaków, możemy również użyć funkcji `mb_strlen()` do znajdowania długości łańcucha w oparciu o kodowanie znaków. Dzięki temu unikniemy problemów związanych z różnicami między encodowaniem na różnych serwerach.

```
<?php
$hello = "Hello World!";
echo mb_strlen($hello);
?>
```

> Output: 12

## Deep Dive:
Dopóki bezpośrednio nie pracujesz z tekstem, nie ma wiele powodów do ręcznego znajdowania długości łańcucha. W przeciwnym razie może to być przydatne w celu optymalizacji aplikacji, zwłaszcza w przypadku operacji na dużych łańcuchach znaków. W takim przypadku warto korzystać z funkcji `mb_strlen()` lub `strlen()` w zależności od potrzeb.

## Zobacz także:
- Dokumentacja PHP dla funkcji `strlen()`: http://php.net/manual/pl/function.strlen.php
- Dokumentacja PHP dla funkcji `mb_strlen()`: http://php.net/manual/pl/function.mb-strlen.php