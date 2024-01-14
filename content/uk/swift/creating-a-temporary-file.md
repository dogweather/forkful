---
title:                "Swift: Створення тимчасового файлу."
simple_title:         "Створення тимчасового файлу."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Чому

Створення тимчасового файлу є важливою частиною програмування в Swift, оскільки воно дозволяє зберігати тимчасові дані, які можуть бути використані для обробки або передачі у більшій програмі. Це дуже зручно для тимчасової збереження результатів обчислень або для зберігання даних, які потрібні для подальшої обробки.

# Як створити тимчасовий файл

Існує кілька способів створення тимчасового файлу в Swift, але основний підхід полягає у використанні класу `FileManager`. Нижче подані приклади коду і його результату, які допоможуть вам зрозуміти, як це працює.

```Swift
// Імпортування модуля FileManager
import Foundation

// Створення об'єкту FileManager
let fileManager = FileManager.default

// Створення шляху до тимчасової папки
let temporaryDirectory = fileManager.temporaryDirectory

// Створення імені тимчасового файлу
let tempFileName = "file.txt"

// Повна шляху до тимчасового файлу
let tempFilePath = temporaryDirectory.appendingPathComponent(tempFileName)

// Створення тимчасового файлу з текстовим вмістом
try "Це тимчасовий файл".write(to: tempFilePath, atomically: true, encoding: .utf8)
```

Результат:

```
/tmp/file.txt
```

Зверніть увагу, що тимчасовий файл буде збережений у папці `/tmp`, але це може відрізнятися залежно від операційної системи.

# Поглиблене дослідження

Створення тимчасових файлів у Swift може бути більш складною задачею, особливо якщо необхідно зберігати більш складні дані. Для цього існує багато інших методів і класів, таких як `NSFileHandle` або `NSFileManagerDelegate`, які дозволяють більше контролю над процесом створення тимчасових файлів. Тому, якщо ви плануєте використовувати тимчасові файли у ваших проектах, рекомендуємо ретельно вивчити Swift документацію та практикуватися для кращого розуміння цієї теми.

# Додаткові ресурси

- https://developer.apple.com/documentation/foundation/filemanager
- https://www.londonappdeveloper.com/how-to-create-a-temporary-file-in-swift/
- https://www.raywenderlich.com/950-sandboxing-and-the-file-system-in-swift
- https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-file-securely-using-filename-unique

# Дивись також

- https://www.programmersought.com/article/89623881730/