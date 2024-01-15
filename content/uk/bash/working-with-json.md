---
title:                "Робота з json"
html_title:           "Bash: Робота з json"
simple_title:         "Робота з json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Причина

Зараз є багато даних у форматі JSON (JavaScript Object Notation), що користується популярністю у багатьох веб-додатках та сервісах. Тож, для будь-якого програміста важливо мати знання і навички роботи з цим форматом даних. В цій статті ми розглянемо, як використовувати Bash для роботи з JSON.

## Як це зробити

Для початку, нам знадобиться установити пакет jq, що дозволить нам працювати з JSON в Bash.

```Bash
sudo apt install jq
```

Тепер ми можемо розпочати роботу! Наприклад, давайте розглянемо такий JSON файл:

```Bash
{"name":"John", "age": 25, "country": "Ukraine"}
```

### Отримання даних

Щоб отримати значення конкретного ключа, скористаємось командою jq з опцією ```-r``` для того, щоб вивести результат без подвійних лапок.

```Bash
jq -r '.name' example.json
```

Результат:

```Bash
John
```

### Оновлення даних

Змінити значення поля в JSON можна за допомогою команди ```jq '.field = "new value"'```.

```Bash
jq '.age = "30"' example.json
```

Результат:

```Bash
{"name":"John", "age": 30, "country": "Ukraine"}
```

### Запис у новий файл

Щоб записати результат в інший файл, скористаємось командою ```jq '.field = "new value"' example.json > new_example.json```.

## Глибоке підглиблення

Але що робити, якщо наш JSON файл містить вкладені об'єкти? Для цього використовуються спеціальні команди, які дозволяють обробляти вкладені даних.

Наприклад, у нас є наступний JSON файл:

```Bash
{"name":"John", "age": 25, "address": {"city": "Kyiv", "country": "Ukraine"}}
```

Щоб отримати значення певного поля з вкладеного об'єкту, ми можемо використати команду ```jq '.address.city'```.

Результат:

```Bash
Kyiv
```

Також можна використовувати фільтри для знаходження певних значень. Наприклад, команда ```jq '.[].country'``` показуватиме країну кожного члена списку даних.

## Дивись також

* [Офіційна документація jq](https://stedolan.github.io/jq/)
* [Стаття про створення JSON у Bash](https://www.scriptinglibrary.com/articles/creating-json-in-bash)
* [Стаття про парсинг JSON за допомогою Bash](https://www.linuxjournal.com/content/parsing-json-bash)