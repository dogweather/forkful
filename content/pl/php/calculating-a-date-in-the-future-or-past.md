---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "PHP: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to kolejny krok po prostym zapisywaniu daty. Programiści robią to z wielu powodów, takich jak planowanie wydarzeń, śledzenie czasu trwania czegoś, lub nawet proste wyliczanie czasu pozostałego do następnego urodzin.

## Jak to zrobić:
W PHP, możemy to zrobić za pomocą wbudowanej klasy DateTime. 

Zacznijmy od ustawienia daty na dzisiaj.

```PHP
$date = new DateTime();
echo $date->format('Y-m-d');
```

Jeżeli chcemy przesunąć datę o 1 tydzień do przodu, możemy to zrobić za pomocą metody `modify`.

```PHP
$date->modify('+1 week');
echo $date->format('Y-m-d');
```

A co jeśli chcesz się cofnąć w czasie? To jest równie proste.

```PHP
$date->modify('-1 year');
echo $date->format('Y-m-d');
```

## Głębsze zrozumienie

Historia obliczeń dat sięga aż do starożytnych Sumerów, którzy używali złożonego systemu kalendarza. 

Alternatywą dla kodu PHP mogą być biblioteki firm zewnętrznych, takie jak Carbon w Laravel, które oferują jeszcze więcej funkcji i są często łatwiejsze w obsłudze.

W przypadku obliczania przyszłych i przeszłych dat warto zwrócić uwagę na strefy czasowe, rok przestępny i formaty dat. PHP zazwyczaj radzi sobie z tym dobrze, ale gdy programista musi pracować na konkretnej dacie, zawsze warto to sprawdzić.

## Zobacz także

- [Dokumentacja PHP DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Biblioteka Carbon w Laravel](https://carbon.nesbot.com/docs/)
- [Dokumentacja PHP o strefach czasowych](https://www.php.net/manual/en/timezones.php)
- [Wikipedia: Rok przestępny](https://pl.wikipedia.org/wiki/Rok_przest%C4%99pny)