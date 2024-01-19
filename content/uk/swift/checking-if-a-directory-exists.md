---
title:                "Перевірка наявності директорії"
html_title:           "Go: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Перевірка наявності каталогу - це процес визначення, чи існує певний каталог у файловій системі. Програмісти це роблять, щоб запобігти помилкам при спробі читання або запису в неіснуючий каталог.

## Як це зробити:

В Swift можна використовувати клас FileManager для цієї задачі. Ось приклад коду:

```Swift
let fileManager = FileManager.default

if let documentsDirectory = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first {
    let path = documentsDirectory.appendingPathComponent("MyDirectory")
    
    var isDir : ObjCBool = false
    if fileManager.fileExists(atPath: path.path, isDirectory:&isDir) {
        if isDir.boolValue {
            // Directory exists
            print("Каталог існує")
        } else {
            print("Це не каталог")
        }
    } else {
        print("Каталогу не існує")
    }
}
```

## Поглиблений занурення:

1. **Історичний контекст.** З часів Unix існують різні методи перевірки існування каталогу, більшість з яких включають використання системного виклику stat або його варіантів.
2. **Альтернативи.** Є переваги над легковаговими методами, такими як використання команди 'ls' через запуск процесу Shell, але вони непортабельні і потенційно дозволяють вразливості оболонки.
3. **Деталі реалізації.** `fileExists(atPath:isDirectory:)` використовує системний виклик stat за лаштунками. Якщо ви зрозумієте деталі цього, то буде легше зрозуміти, як це працює.

## Дивіться також:

Пов'язані джерела включають:

- Офіційну документацію Apple по FileManager:
  [Документація FileManager](https://developer.apple.com/documentation/foundation/filemanager)

- Stackoverflow про обробку помилок під час роботи з файлами та каталогами: 
  [Обробка помилок функції FileManager](https://stackoverflow.com/questions/24355866/how-to-catch-exception-thrown-by-nsfilemanager-defaultmanager-contents-of-dire)