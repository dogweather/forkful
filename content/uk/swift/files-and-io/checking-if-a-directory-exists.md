---
title:                "Перевірка наявності директорії"
aliases: - /uk/swift/checking-if-a-directory-exists.md
date:                  2024-02-03T19:09:01.074798-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Перевірка на наявність директорії у файловій системі є важливою для управління структурами файлів з ваших застосунків на Swift. Це завдання дозволяє розробникам перевіряти наявність директорій перед спробами читання з них або запису в них, таким чином уникаючи можливих помилок під час виконання.

## Як це зробити:

Фреймворк Foundation в Swift надає клас `FileManager`, який має методи для управління файловою системою. Ви можете використовувати `FileManager` для перевірки наявності директорії. Ось фрагмент коду, що показує, як це зробити:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/шлях/до/вашої/директорії"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Директорія існує")
} else {
    print("Директорія не існує")
}
```

Однак, це перевіряє як файли, так і директорії. Якщо ви спеціально хочете перевірити, що директорія існує, вам потрібно передати вказівник на булеве значення в `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/шлях/до/вашої/директорії"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Директорія існує")
} else {
    print("Директорія не існує")
}
```

### Використання сторонньої бібліотеки

Станом на зараз, для перевірки наявності директорії в Swift зазвичай не потрібно використовувати сторонні бібліотеки завдяки міцності класу `FileManager`. Однак, для більш складних маніпуляцій і перевірок файлів, бібліотеки, як-от **Files** від John Sundell, надають більш зручний для Swift API.

Ось як ви могли б це використати:

Спочатку, додайте Files до вашого проекту через Swift Package Manager.

Потім, ви можете перевірити наявність директорії таким чином:

```swift
import Files

do {
    _ = try Folder(path: "/шлях/до/вашої/директорії")
    print("Директорія існує")
} catch {
    print("Директорія не існує")
}
```

Примітка: Оскільки сторонні бібліотеки можуть змінюватись, завжди консультуйтеся з останньою документацією для інструкцій і кращих практик використання.
