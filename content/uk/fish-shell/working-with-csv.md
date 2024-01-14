---
title:                "Fish Shell: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Робота з CSV-файлами є невід'ємною частиною багатьох програм і проектів. Цей формат даних є популярним і легким у використанні, тому вміння працювати з ним володіє великою цінністю для розробників.

## Як працювати з CSV в Fish Shell

Щоб почати роботу з CSV-файлами у Fish Shell, спочатку потрібно завантажити пакет [csvkit](https://github.com/wireservice/csvkit). Після цього можна почати працювати з даними за допомогою таких команд:

- `csvcut`: вибирає вказані стовпці з CSV-файлу і показує їх на екрані
- `csvlook`: форматує дані з CSV-файлу в зручний для читання вигляд
- `csvstat`: виводить статистику щодо даних у CSV-файлі

Нижче наведено приклад використання цих команд:

```
Fish Shell - працюємо з CSV

Файл dogs.csv містить дані про кількість собак у кожному місті:

```Fish Shell
>> csvcut -c City,Dogs dogs.csv
City,Dogs
London, 45000
New York, 30000
Paris, 25000
```

Використання csvlook дозволяє подивитися на ці дані у більш зручному форматі:

```Fish Shell
>> csvlook dogs.csv
|----------+-------|
|  City    | Dogs  |
|----------+-------|
|  London  | 45000 |
|----------+-------|
| New York | 30000 |
|----------+-------|
|  Paris   | 25000 |
|----------+-------|
```

Завершення роботи з CSV-файлом може виглядати так:

```Fish Shell
>> csvcut -c City,Dogs dogs.csv > new_dogs.csv
> cat new_dogs.csv
City,Dogs
London, 45000
New York, 30000
Paris, 25000
```

## Поглиблене вивчення

Якщо ви зацікавлені у більш поглибленому вивченні роботи з CSV-файлами в Fish Shell, рекомендуємо ознайомитися з [офіційною документацією](http://csvkit.readthedocs.io/en/1.0.2/) для більш детальної інформації та прикладів використання.

## Дивіться також

- [Офіційна документація csvkit](http://csvkit.readthedocs.io/en/1.0.2/)
- [Параметри команд для роботи з CSV (англ.)](https://csvkit.readthedocs.io/en/1.0.2/scripts/)
- [Пакет csvkit у Fish Shell (англ.)](https://github.com/ffish/csvkit)
- [Запис уроку на YouTube про роботу з CSV (укр.)](https://www.youtube.com/watch?v=wnwVc6YbPQQ)