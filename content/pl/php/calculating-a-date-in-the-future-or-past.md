---
title:                "PHP: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie dat w przyszłości lub przeszłości może być niezbędne w wielu projektach programistycznych. Może to być potrzebne do wyświetlania ważnych wydarzeń lub terminów, planowania zadań lub wyliczania wieku. Dzięki PHP i jego funkcjom związanym z datami, obliczanie dat staje się łatwe i wygodne.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, możemy skorzystać z funkcji "strtotime()" w PHP. Przykładowy kod wyglądałby następująco:

```PHP
$date = strtotime("+1 week"); // dodanie tygodnia do aktualnej daty
echo date("Y-m-d", $date); // wyświetli 2021-10-14
```

Możemy również wykorzystać funkcję "mktime()" do utworzenia daty na podstawie podanych argumentów, takich jak rok, miesiąc i dzień.

```PHP
$date = mktime(0, 0, 0, 12, 25, 2021); // utworzenie daty: 25 grudnia 2021
echo date("l", $date); // wyświetli Saturday
```

Możemy także użyć funkcji "date_diff()", aby obliczyć różnicę między dwoma datami. Przykładowy kod wyglądałby tak:

```PHP
$firstDate = date_create("2021-01-01");
$secondDate = date_create("2021-12-31");
$diff = date_diff($firstDate, $secondDate);
echo $diff->format("%R%a days"); // wyświetli +364 days
```

## Deep Dive

W PHP mamy do dyspozycji wiele funkcji związanych z datami, takich jak "strtotime()", "mktime()", "date()", "date_create()" czy "date_diff()". Są one bardzo przydatne w obliczaniu dat w przyszłości lub przeszłości, a także wyświetlaniu lub formatowaniu dat w różny sposób.

Ważną rzeczą, na którą należy zwrócić uwagę, jest użycie odpowiednich formatów daty lub czasu. W przeciwnym razie mogą pojawić się nieoczekiwane rezultaty lub błędy w kodzie. Dobrą praktyką jest także użycie funkcji "setlocale()" do ustawienia odpowiedniej lokalizacji, szczególnie jeśli wyświetlamy daty w innym języku niż angielski.

## Zobacz także

- Oficjalna dokumentacja PHP dotycząca funkcji związanych z datami: [https://www.php.net/manual/en/ref.datetime.php](https://www.php.net/manual/en/ref.datetime.php)
- Przewodnik po funkcjach związanych z datami w PHP: [https://www.w3schools.com/php/php_date.asp](https://www.w3schools.com/php/php_date.asp)
- Przydatny poradnik dotyczący dat w PHP: [https://www.geeksforgeeks.org/dates-and-time-in-php/](https://www.geeksforgeeks.org/dates-and-time-in-php/)