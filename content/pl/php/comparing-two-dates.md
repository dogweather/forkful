---
title:                "PHP: Porównywanie dwóch dat"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy porównać dwie daty, np. aby sprawdzić, czy pewien wydarzenie ma miejsce przed czy po innej dacie. W tym artykule pokażemy jak w prosty sposób porównać dwie daty w języku PHP.

## Jak to zrobić

Porównanie dwóch dat w PHP jest bardzo proste dzięki wykorzystaniu funkcji `strtotime()` oraz operatorów logicznych. Poniżej znajduje się przykładowy kod, który porównuje dwie daty i zwraca odpowiedni komunikat:

```PHP
$date1 = strtotime("01-01-2021");
$date2 = strtotime("05-01-2021");

if ($date1 < $date2) {
   echo "Pierwsza data jest wcześniejsza niż druga data.";
} else {
   echo "Pierwsza data jest późniejsza lub równa drugiej dacie.";
}
```
**Output:** Pierwsza data jest wcześniejsza niż druga data.

W przykładzie powyżej najpierw używamy funkcji `strtotime()` aby przekonwertować daty na wartości liczbowe, które można porównać za pomocą operatorów logicznych. Funkcja ta jest bardzo przydatna, ponieważ pozwala nam na porównywanie dat w różnych formatach.

Możemy również porównywać daty z obecnym czasem, używając funkcji `time()`:

```PHP
$date = strtotime("03-01-2021");
$today = time();

if ($date < $today) {
   echo "Data jest wcześniejsza niż obecny czas.";
} else {
   echo "Data jest późniejsza lub równa obecnemu czasowi.";
}
```
**Output:** Data jest wcześniejsza niż obecny czas.

## Głębszy wgląd

W języku PHP istnieje również funkcja `date_diff()`, która pozwala na dokładniejsze porównywanie dat przez uwzględnienie różnych jednostek czasu. Na przykład:

```PHP
$date1 = date_create("30-01-2021");
$date2 = date_create("05-02-2021");

$diff = date_diff($date1, $date2);
echo "Różnica między datami wynosi {$diff->format('%d')} dni.";
```
**Output:** Różnica między datami wynosi 6 dni.

Funkcja `date_diff()` zwraca obiekt `DateInterval`, który zawiera wiele przydatnych metod, dzięki którym możemy uzyskać różnicę w różnych jednostkach czasu.

## Zobacz także

- Przewodnik po funkcjach daty i czasu w PHP: https://www.php.net/manual/en/datetime.fun
- Przykłady użycia funkcji strtotime(): https://www.w3schools.com/php/func_date_strtotime.asp
- Dokumentacja funkcji date_diff(): https://www.php.net/manual/en/datetime.diff.php