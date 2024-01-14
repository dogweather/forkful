---
title:    "PHP: Obliczanie daty w przyszłości lub przeszłości."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Dlaczego

Czasem może być potrzebne obliczenie daty w przyszłości lub w przeszłości w celu rozwiązania problemów programistycznych lub w celach biznesowych.

##Jak to zrobić

Aby obliczyć datę w przyszłości lub w przeszłości w języku PHP, można wykorzystać funkcję `strtotime()`. Przyjmując jako pierwszy argument określoną datę oraz drugi argument w postaci "s" (sekund), "m" (minut), "h" (godzin), "d" (dni), można precyzyjnie określić, o ile chcemy przesunąć datę.

Przykład:

```PHP
$date = strtotime('+3 days'); // Wartość domyślna dla drugiego argumentu to "s"
echo date('Y-m-d', $date); // 2021-11-29
```

Można również określić datę startową poprzez przekazanie jej jako trzeciego argumentu w postaci timestampu.

Przykład:

```PHP
$startDate = strtotime('2021-12-01');
$date = strtotime('+2 weeks', $startDate); // Przesuwamy datę o 2 tygodnie od 2021-12-01
echo date('Y-m-d', $date); // 2021-12-15
```

##Vertiefung

Funkcja `strtotime()` jest bardzo przydatna do obliczania dat w przyszłości lub w przeszłości w języku PHP, ale warto pamiętać o kilku rzeczach. Pierwszą z nich jest fakt, że przy niektórych kombinacjach daty i drugiego argumentu, wynik może być nieprecyzyjny lub w ogóle niepoprawny. W takim przypadku warto skorzystać z innej dostępnej funkcji, jak na przykład `DateTime` lub biblioteki Carbon.

Kolejnym elementem jest fakt, że funkcja `strtotime()` bazuje na lokalnych ustawieniach czasowych, dlatego wynik może się różnić w zależności od strefy czasowej, w której działa skrypt. Dlatego ważne jest ustawienie odpowiedniej strefy czasowej przy użyciu funkcji `date_default_timezone_set()`.

##Zobacz również

- [Dokumentacja funkcji strtotime() w języku PHP](https://www.php.net/manual/en/function.strtotime.php)
- [Poradnik dla programistów: jak wykorzystać funkcję strtotime() w języku PHP](https://www.php.net/manual/en/function.strtotime.php)
- [Oficjalna strona biblioteki Carbon](https://carbon.nesbot.com/)