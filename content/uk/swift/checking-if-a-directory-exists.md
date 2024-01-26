---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:58:56.478251-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Перевірка наявності директорії – це процес визначення, чи існує певний каталог у файловій системі. Програмісти роблять це, щоб уникнути помилок при спробі доступу або модифікації файлів в неіснуючій папці.

## Як це зробити:
Swift надає клас `FileManager` для роботи з файловою системою. Ось як перевірити існування директорії:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/directory"

if fileManager.fileExists(atPath: path, isDirectory: UnsafeMutablePointer<ObjCBool>.allocate(capacity: 1)) {
    print("Директорія існує.")
} else {
    print("Директорії не існує.")
}
```

## Поглиблений огляд:
Перевірка наявності папок у Swift використовує `FileManager`, унаслідуваний від попередніх Apple API, таких як NeXTSTEP та Cocoa. Інші мови мають свої інструменти, наприклад, `os.path.exists()` у Python. Попри те, що попередні версії Swift могли вимагати більш складних методів для перевірки існування папок, зараз `FileManager` є стандартним та ефективним рішенням. Також варто зауважити, що `fileExists(atPath:)` може бути використаний для файлів та силок на них, тому перевірка чи об'єкт є директорією важлива.

## Ознайомтеся також:
- [Apple Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [NSHipster article on File System Operations in Swift](https://nshipster.com/filemanager/)
- [Ray Wenderlich's guide to working with files in iOS](https://www.raywenderlich.com/1918-beginning-ios-file-system-programming-in-swift)
