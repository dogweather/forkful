---
title:    "PHP: Porównywanie dwóch dat"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego 

Porównywanie dwóch dat to ważny element w programowaniu PHP. Dzięki temu możemy kontrolować, które dane są nowsze, a które są starsze. Jest to szczególnie przydatne w wielu aplikacjach, takich jak systemy rezerwacji, kalendarze czy systemy raportowania.

## Jak to zrobić

Aby porównać dwie daty w PHP, musimy najpierw przekonwertować je na typ `DateTime`. Możemy to zrobić przy użyciu funkcji `strtotime()` lub poprzez podanie daty jako parametru konstruktora `DateTime`. Następnie możemy użyć metody `diff()` w celu porównania dwóch obiektów `DateTime`. Sprawdźmy przykład poniżej:

```PHP
$date1 = new DateTime("2020-08-15");
$date2 = new DateTime("2020-09-05");

$diff = $date1->diff($date2);

echo $diff->format("%r %a days"); 
// output: +21 days
```

W powyższym przykładzie najpierw tworzymy dwa obiekty `DateTime` dla naszych dat - 15 sierpnia 2020 i 5 września 2020. Następnie wywołujemy metodę `diff()` dla obiektu `$date1`, przekazując jako parametr obiekt `$date2`. W efekcie otrzymujemy różnicę między tymi dwoma datami w formacie `DateInterval`. Aby wyświetlić wynik w bardziej czytelnej formie, wykorzystujemy metodę `format()`, gdzie `%r` oznacza znak plus lub minus (w zależności od tego, czy jedna data jest wcześniejsza niż druga) i `%a` oznacza liczbę dni. Dzięki temu widzimy, że różnica między tymi dwoma datami wynosi 21 dni.

## Pogłębione informacje

Porównywanie dwóch dat może być trudne, szczególnie jeśli bierzemy pod uwagę różne strefy czasowe lub formaty dat. W takich przypadkach, zawsze powinniśmy się upewnić, że nasze daty są przekonwertowane na ten sam format przed użyciem metody `diff()`.

## Zobacz również

Sprawdź inne przydatne funkcje i metody do pracy z datami w PHP:

- [Funkcja `strtotime()` w PHP](https://www.php.net/manual/pl/function.strtotime.php)
- [Konstruktor `DateTime()` w PHP](https://www.php.net/manual/pl/datetime.construct.php)
- [Metoda `diff()` w PHP](https://www.php.net/manual/pl/datetime.diff.php)
- [Klasa `DateInterval()` w PHP](https://www.php.net/manual/pl/class.dateinterval.php)