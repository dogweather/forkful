---
title:                "PHP: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Czemu

Obliczanie daty w przyszłości lub w przeszłości jest często niezbędne w programowaniu PHP, ponieważ pozwala na dynamiczne określanie czasu w aplikacjach internetowych. Może być przydatne, na przykład w przypadku generowania faktur, harmonogramowania zadań lub wyświetlania powiadomień.

## Jak to zrobić

```PHP
$dni = 7;
$data = date('Y-m-d', strtotime("+$dni days"));
echo "Za $dni dni będzie $data";
```
W powyższym przykładzie użyto funkcji `date()` i `strtotime()` do wyliczenia daty, która jest dniem z przeszłości lub przyszłości w stosunku do bieżącej daty. Następnie wyświetlono wynik w formacie `YYYY-MM-DD`, ale można dostosować to według własnych preferencji.

```PHP
$rok = 2021;
$miesiac = 12;
$data = date('Y-m-d', strtotime("$rok-$miesiac-01"));
echo "Pierwszy dzień miesiąca $miesiac w roku $rok to $data";
```
W powyższym przykładzie wykorzystano zmienne, aby obliczyć pierwszy dzień wybranego miesiąca i roku. Można również dodawać lub odejmować dni, tygodnie, miesiące i lata do daty i określać konkretne dni tygodnia, na których chcemy otrzymać wynik.

## Wnikliwa analiza

Przy obliczaniu dat w przyszłości lub przeszłości należy pamiętać o różnych formatach dat i ustawieniach strefy czasowej. Należy również zwrócić uwagę na to, czy dany rok jest przestępny lub czy dany miesiąc ma 30 czy 31 dni. W przypadku wystąpienia problemów z obliczeniami dat, warto skorzystać z dostępnych bibliotek PHP, takich jak `DateTime` lub `Carbon`, które ułatwiają pracę z datami.

## Zobacz również

- [Dokumentacja PHP o funkcji date()](http://php.net/manual/en/function.date.php)
- [Dokumentacja PHP o funkcji strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [Biblioteka DateTime w PHP](https://www.php.net/manual/en/class.datetime.php)
- [Biblioteka Carbon w PHP](https://carbon.nesbot.com/)