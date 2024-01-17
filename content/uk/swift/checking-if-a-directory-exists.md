---
title:                "Перевірка наявності каталогу"
html_title:           "Swift: Перевірка наявності каталогу"
simple_title:         "Перевірка наявності каталогу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Що & Чому?
Перевірка існування каталогу - це процес перевірки, чи існує вказаний каталог в системі файлів. Програмісти часто використовують цю перевірку для того, щоб переконатися, що необхідний каталог існує, перед тим як продовжувати виконання коду.

Як?
Використовувати перевірку існування каталогу досить просто. Просто створіть об'єкт типу `FileManager`, використовуючи ключове слово `let`, і використовуйте його метод `fileExists(atPath:)`, передаючи йому шлях до каталогу, який ви хочете перевірити. Цей метод поверне значення `true` або `false`, в залежності від того, чи існує цей каталог.

```Swift
let fileManager = FileManager()
let directory = "/Users/User/Documents" // приклад шляху до каталогу
if fileManager.fileExists(atPath: directory) {
    print("Каталог існує")
} else {
    print("Каталог не існує")
}
```

Глибока занурення
Перевірка існування каталогу не є новою функцією, і її можна знайти в багатьох мовах програмування. У Swift цю перевірку можна здійснити за допомогою методу `fileExists(atPath:)` або використовуючи `if let` конструкцію для перевірки наявності каталогу перед його використанням. Якщо ви хочете перевірити існування файлу, а не каталогу, ви можете скористатися методами `fileExists(atPath:)` або `fileExists(atPath:isDirectory:)`, передаючи `isDirectory` аргумент зі значенням `false`.

Подивіться також
Якщо ви хочете дізнатися більше про використання `FileManager` і роботу з файловою системою, подивіться на цей офіційний документацію від Apple: https://developer.apple.com/documentation/foundation/filemanager

Також ви можете користуватися `NSFileManager` для роботи із файловою системою в Objective-C коді. Більше інформації ви знайдете тут: https://developer.apple.com/documentation/foundation/nsfilemanager

Щоб дізнатися більше про роботу з файловою системою в Swift, можна вивчати це завдання на платформі Codecademy: https://www.codecademy.com/learn/paths/learn-swift