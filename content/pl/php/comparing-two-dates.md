---
title:                "PHP: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie danych jest ważnym elementem w programowaniu, szczególnie w PHP. Nie tylko pozwala ono na sprawdzenie czy dwa obiekty są identyczne, ale także może pomóc w określeniu, który z nich jest większy lub mniejszy. W tym artykule dowiesz się, jak porównywać dwie daty w PHP i dlaczego jest to istotne w codziennej pracy programisty.

## Jak to zrobić

Mając do dyspozycji dwa obiekty daty, można wykorzystać funkcję `strtotime` do zamiany ich na wartości numeryczne, które można bezpośrednio porównać. Przykładowo:

```
$date1 = '2021-01-01';
$date2 = '2020-12-31';

if (strtotime($date1) > strtotime($date2)) {
  echo '$date1 jest późniejsze niż $date2';
} elseif (strtotime($date1) < strtotime($date2)) {
  echo '$date1 jest wcześniejsze niż $date2';
} else {
  echo 'Obie daty są identyczne';
}
```

W powyższym przykładzie, funkcja `strtotime` zamienia daty na liczby, które następnie są porównywane za pomocą operatorów większe lub mniejsze. Warto jednak zwrócić uwagę na to, że powyższy kod może nie działać poprawnie dla daty 29 lutego w latach przestępnych. W takim przypadku można zastosować funkcję `DateTime` oraz jej metodę `diff`, która zwraca różnicę między dwoma obiektami daty. Przykładowo:

```
$date1 = new DateTime('2020-02-28');
$date2 = new DateTime('2020-02-29');

$diff = $date1->diff($date2);

if ($diff->invert) {
  echo '$date1 jest wcześniejsze niż $date2';
} elseif ($diff->days == 0) {
  echo 'Obie daty są identyczne';
} else {
  echo '$date1 jest późniejsze niż $date2';
}
```

W tym przypadku, metoda `diff` zwraca obiekt `DateInterval`, który zawiera informację o różnicy między datami oraz wskazuje, czy pierwsza data jest wcześniejsza czy późniejsza niż druga.

## Wnikliwe spojrzenie

Porównywanie dat jest często potrzebne w przypadku sortowania danych lub wybierania rekordów z bazy danych. Warto również pamiętać o stosowaniu odpowiednich funkcji do sprawdzania czy rok jest przestępny czy czy data jest poprawna. W przypadku bardziej zaawansowanych problemów z datami, warto skorzystać z bibliotek takich jak Carbon, które znacznie ułatwiają operacje na datach w PHP.

## Zobacz też

- [Dokumentacja PHP - Porównywanie dat](https://www.php.net/manual/en/datetime.diff.php)
- [Dokumentacja PHP - Przetwarzanie dat](https://www.php.net/manual/en/datetime.createfromformat.php)
- [Oficjalna strona biblioteki Carbon](http://carbon.nesbot.com/)