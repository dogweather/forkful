---
title:    "Gleam: Створення тимчасового файлу"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу є важливою частиною програмування, оскільки це дозволяє зберегти дані тимчасово на протязі виконання програми.

## Як

Найпростішим способом створення тимчасового файлу в Gleam є використання функції ```tempfile.create()```. Ось приклад коду, який демонструє це:

```Gleam
tempfile.create() |> Ok
```

Ви можете визначити певну назву для тимчасового файлу, передаючи її як аргумент до функції:

```Gleam
tempfile.create("my_file") |> Ok
```

Якщо ви бажаєте вказати шлях для збереження тимчасового файлу, це також можливо:

```Gleam
tempfile.create("my_file", "/home/user/temp") |> Ok
```

Якщо ви хочете отримати шлях до створеного тимчасового файлу, ви можете використовувати функцію ```tempfile.get_path()```:

```Gleam
tempfile.create("my_file") |> Ok
tempfile.get_path("my_file") // поверне шлях до файлу
```

Крім того, ви можете використовувати функцію ```tempfile.delete()``` для видалення створеного тимчасового файлу.

## Глибоке занурення

При створенні тимчасового файлу в Gleam, він буде автоматично видалений, коли програма завершить своє виконання. Це дозволяє заощадити простір на диску та полегшити управління тимчасовими файлами.

## Дивитися також

- [Документація з функції tempfile.create()](https://gleam.run/modules/std/tempfile/#create)
- [Документація з функції tempfile.get_path()](https://gleam.run/modules/std/tempfile/#get_path)
- [Документація з функції tempfile.delete()](https://gleam.run/modules/std/tempfile/#delete)