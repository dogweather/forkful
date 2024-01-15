---
title:                "Перевірка наявності директорії"
html_title:           "Swift: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# З чого почати

If you are a Swift programmer, you probably know that working with files and directories is crucial for many projects. And sometimes, we need to check if a certain directory exists. In this article, we will learn why and how to do it in Swift.

## Чому

Перевірка існування директорії може бути необхідною для безпечного роботи з файлами та даними. Наприклад, перед записом файлу або перед читанням з існуючого каталогу, ми повинні перевірити, чи дійсно існує ця директорія. Також це допомагає уникнути збоїв в програмі та перешкоджає втраті даних.

## Як це зробити

Ми можемо використовувати функцію `FileManager` з Swift для перевірки існування директорії. Наприклад, ми хочемо перевірити, чи існує директорія "Documents" нашого користувача. Ми можемо використовувати такий код:

```Swift
let fileManager = FileManager.default
let documentsDirectory = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0]

if fileManager.fileExists(atPath: documentsDirectory.path) {
    print("Директорія існує!")
} else {
    print("Директорія не існує!")
}
```

Це зробить пошук першої директорії для документів у кореневому каталозі користувача, а потім перевірить, чи існує вона за допомогою функції `fileExists(atPath:)`. Якщо директорія існує, ви побачите по виконанні коду повідомлення "Директорія існує!" у консолі.

Можна також використовувати функцію `fileExists(atPath:)` для перевірки інших каталогів або файлів у вашій програмі. Наприклад, ви можете перевірити, чи існує певний файл у "Downloads" або "Photos" директорії.

## Глибоке погруження

Якщо ви хочете попередити створення дублікатів файлів або директорій, вам потрібно дізнатися, як перевірити існування певного шляху. Ми можемо це зробити за допомогою функції `fileExists(atPath:)` та класу `URL`. Розглянемо приклад:

```Swift
let fileManager = FileManager.default
let documentsDirectory = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0]
let myFileURL = documentsDirectory.appendingPathComponent("text_file.txt")

if fileManager.fileExists(atPath: myFileURL.path) {
    print("Файл існує в директорії!")
} else {
    print("Файл не існує в директорії!")
}
```

Тут ми використали функцію `appendingPathComponent()` для створення нового шляху до нашого файлу "text_file.txt". Потім ми можемо перевірити, чи існує цей файл у дир