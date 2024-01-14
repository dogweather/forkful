---
title:                "PHP: Pobieranie aktualnej daty"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli kodujesz w PHP, prawdopodobnie masz do czynienia z danymi datowymi. Często musimy używać bieżącej daty w naszych aplikacjach, na przykład do wyświetlania świeżych treści lub do ustawiania terminów ważności. W tym artykule dowiesz się, jak w prosty sposób pobrać aktualną datę w języku PHP.

## Jak to zrobić

Jest kilka sposobów na uzyskanie bieżącej daty w PHP. Najpopularniejsze metody to użycie funkcji `date()` oraz obiektu `DateTime`. Oba te sposoby są bardzo proste i szybkie w implementacji.

```PHP
// Użycie funkcji date()
$currentDate = date('Y-m-d');
echo $currentDate; // Output: 2021-09-06

// Użycie obiektu DateTime
$dateTime = new DateTime();
$currentDate = $dateTime->format('Y-m-d');
echo $currentDate; // Output: 2021-09-06
```

Obydwie metody zwracają aktualną datę w formacie `YYYY-MM-DD`, który jest powszechnie używany w bazach danych, dzięki czemu można bez problemu porównywać i sortować daty.

## Głębszy zanurzenie

Jeśli chcesz uzyskać większą kontrolę nad formatem daty lub dodać informacje o czasie, możesz poszerzyć swoją wiedzę o funkcji `date()` lub obiekcie `DateTime`. Możesz również skorzystać z innych przydatnych funkcji, takich jak `strtotime()`, która pozwala na konwersję daty w postaci tekstowej na datę typu Unix Timestamp, czyli liczbę sekund, która minęła od 1 stycznia 1970 roku.

```PHP
// Użycie funkcji date() z dodatkowymi parametrami
$currentDate = date('d-m-Y H:i:s'); // Data i czas
echo $currentDate; // Output: 06-09-2021 14:25:00

// Użycie funkcji strtotime()
$dateString = 'next Monday'; // Następny poniedziałek
$nextMonday = strtotime($dateString);
echo date('d-m-Y', $nextMonday); // Output: 13-09-2021
```

W przypadku obiektu `DateTime` możesz korzystać z różnych metod, takich jak `add()`, `sub()`, `diff()`, które pozwalają na dodawanie, odejmowanie i porównywanie dat.

## Zobacz też

- [Dokumentacja funkcji `date()` w języku PHP](https://www.php.net/manual/en/function.date.php)
- [Dokumentacja klasy `DateTime` w języku PHP](https://www.php.net/manual/en/class.datetime.php)
- [Artykuł na temat dat i czasu w języku PHP](https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-php)