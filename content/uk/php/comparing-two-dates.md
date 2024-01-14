---
title:                "PHP: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

Чому: Порівняння двох дат є важливим аспектом програмування, оскільки він дозволяє виконувати різні операції з датами, такі як порівняння, сортування та обрахунок різниці між ними.

Як: Для порівняння двох дат використовується функція `strtotime ()`, яка перетворює дату в мітку часу. Потім ці мітки часу можна порівнювати за допомогою операторів порівняння (`<`, `>`, `<=`, `>=`, `==`, `!=`).

```PHP 
$firstDate = "2021-04-20";
$secondDate = "2021-05-02";

$firstTimestamp = strtotime($firstDate);
$secondTimestamp = strtotime($secondDate);

if ($firstTimestamp < $secondTimestamp) {
  echo "Перша дата передує другій";
} elseif ($firstTimestamp > $secondTimestamp) {
  echo "Перша дата після другої";
} else {
  echo "Дати рівні";
}
```
Вивід:
```
Перша дата передує другій
```

Глибше: Порівняння дат також можливе за допомогою класу `DateTime`, який надає більше можливостей для маніпулювання датою і часом. Це дозволяє встановити власну зону часу, обчислити проміжок часу між двома датами і багато іншого.

See Also (Дивіться також):
- [Офіційна документація PHP про порівняння дат](https://www.php.net/manual/en/datetime.diff.php)
- [Розбираємося з функцією `strtotime()`](https://code.tutsplus.com/uk/tutorials/building-advanced-calendar-systems-in-php-with-date-and-strtotime--net-3085) 
- [Корисні приклади використання класу `DateTime`](https://www.php.net/manual/en/class.datetime.php)