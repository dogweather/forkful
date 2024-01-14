---
title:    "PHP: Porównywanie dwóch dat"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Wybór odpowiedniej metody porównania dwóch dat jest kluczowy dla prawidłowego działania aplikacji. W tym artykule przyjrzymy się różnym podejściom do tego problemu i jak je zaimplementować w języku PHP.

## Jak to zrobić

Przy comparowaniu dwóch dat w PHP należy pamiętać o kilku ważnych czynnikach. Pierwszym z nich jest konieczność użycia odpowiednich funkcji do przetwarzania dat. W przypadku porównywania dat zapisanych w formacie tekstowym, należy skorzystać z funkcji `strtotime()` do ich konwersji na liczbę. Następnie można użyć `date()` lub `strftime()` w celu porównania dwóch liczb reprezentujących daty.

Przykładowy kod w PHP wykorzystujący te funkcje wyglądałby następująco:

```PHP
$data1 = "2020-01-01";
$data2 = "2020-01-15";

if (strtotime($data1) < strtotime($data2)) {
  echo "Data 1 jest wcześniejsza niż data 2.";
} else {
  echo "Data 2 jest wcześniejsza niż data 1.";
}
```

W powyższym przykładzie, funkcja `strtotime()` przekształca daty do postaci liczb, a następnie porównujemy je za pomocą konstrukcji warunkowej `if`.

Można również wykorzystać wbudowane w PHP klasy do obsługi dat oraz funkcję `DateTime::diff()` do porównania dwóch dat. Przykładowy kod wykorzystujący tę metodę wyglądałby następująco:

```PHP
$data1 = new DateTime("2020-01-01");
$data2 = new DateTime("2020-01-15");

$roznica = $data1->diff($data2);

if ($roznica->invert == 0) {
  echo "Data 1 jest wcześniejsza niż data 2.";
} else {
  echo "Data 2 jest wcześniejsza niż data 1.";
}
```

W powyższym przykładzie, za pomocą funkcji `DateTime::diff()` pobieramy różnicę między dwoma datami i sprawdzamy pole `invert` w obiekcie `DateInterval`. Jeśli jego wartość wynosi 0, to oznacza, że pierwsza data jest wcześniejsza niż druga.

## Deep Dive

Podczas porównywania dat należy zwrócić uwagę na fakt, że niektóre operacje mogą prowadzić do błędów. Przykładowo, porównanie dwóch dat zapisanych w różnych strefach czasowych może dać nieoczekiwane rezultaty. Dlatego ważne jest, aby dokładnie przeanalizować swoje wymagania i wybrać odpowiednie rozwiązanie.

Innym ważnym czynnikiem jest formatowanie danych. W zależności od potrzeb, można wybrać różne formaty zapisu daty w PHP, np. `yyyy-mm-dd` lub `mm/dd/yyyy`. Należy pamiętać, aby używać konsekwentnego formatu i zawsze najpierw przekształcać daty na liczbę lub obiekt, zanim zacznie się porównywać je ze sobą.

## Zobacz także

- [PHP Manual - Porównywanie dat](https://www.php.net/manual/en/function.strtotime.php)
- [PHP Manual - DateTime::diff()](https://www.php.net/manual/en/datetime.diff.php)
- [W3Schools - PHP Date and Time functions](https://www.w3schools.com/php/php_date.asp)