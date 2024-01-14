---
title:                "Gleam: Створення тимчасового файлу"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу є універсальним інструментом у всіх програмувальних мовах, також у Gleam. Тимчасові файли зберігають даний протягом обробки даних, а потім автоматично видаляються, що забезпечує більш ефективне використання ресурсів.

## Як

Для створення тимчасового файлу у Gleam, ми можемо використати вбудовану функцію `gleam_core.file_system.temp_file`. Перш ніж натиснути на жовту кнопку "Запустити" нижче, подумайте про область застосування тимчасового файлу і вставте свої власні дані у зазначений код:

```Gleam
import gleam_core.file_system

file, error = gleam_core.file_system.temp_file()

case (file, error) {
  (Ok(file), _) -> io.print(file.path)
  (_, Err(message)) -> io.print("Помилка: " ++ message)
}
```

Вихідний код українською мовою виглядатиме наступним чином:

```Gleam
імпорт gleam_core.file_system

файл, помилка = gleam_core.file_system.temp_file()

випадок (файл, помилка) {
  (Ok(файл), _) -> io.print(файл.path)
  (_, Err(message)) -> io.print("Помилка: " ++ повідомлення)
}
```

Після запуску програми, ви отримаєте шлях до створеного тимчасового файлу, який можна використовувати подальше для обробки даних.

## Vertlieb

Специфікації Gleam дають можливість більш детально опанувати тему створення тимчасових файлів. Наприклад, ви можете вказати префікс тимчасового файлу або вказати, чи має файл бути створеним у тимчасовій папці чи у вказаній користувацькій папці.

```Gleam
import gleam_core.file_system

file, error = gleam_core.file_system.temp_file(
  prefix: "temp-",
  dir: "/home/user/documents"
)

case (file, error) {
  (Ok(file), _) -> io.print(file.path)
  (_, Err(message)) -> io.print("Помилка: " ++ message)
}
```

Українською мовою, той же код виглядатиме так:

```Gleam
імпорт gleam_core.file_system

файл, помилка = gleam_core.file_system.temp_file(
  prefix: "temp-",
  dir: "/доминиця/користувач/документи"
)

випадок (файл, помилка) {
  (Ok(файл), _) -> io.print(файл.path)
  (_, Err(message)) -> io.print("Помилка: " ++ повідомлення)
}
```

## Дивіться також

- [Офіційна документація Gleam](https://gleam.run)
- [Стаття про працю з файловою системою у Gleam](https://dev.to/codedge/working-with-files-in-gleam-5ain)
- [Кодовий приклад зі створенням тимчасового файлу на GitHub](https://github