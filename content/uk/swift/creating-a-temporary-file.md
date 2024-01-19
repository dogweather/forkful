---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

**## Що й чому?**

Створення тимчасових файлів - це процес обміну даними між різними частинами коду або зберігання проміжних результатів виконання кожного сегменту. Програмісти це роблять, щоб всандалити проект, який працює в багатозадачному режимі або виконувати багатокроковые оперативні завдання.

**## Як це зробити:**

Розглянемо реалізацію прикладів на Swift:

```Swift
import Foundation

let temporaryDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent(UUID().uuidString)

try "Hello, World!".write(to: temporaryFileURL, atomically: true, encoding: .utf8)

print(try String(contentsOf: temporaryFileURL))
```
Код вище створює тимчасовий файл з випадковим ім'ям і використовує його для збереження рядка "Hello, World!". Потім він читає та друкує строку з цього файлу.

**## Глибше занурення**

1) **Історичний контекст:** Тимчасові файли стали поширеними у програмуванні шляхом забезпечення спосібу обробки великих обсягів даних, які не можуть бути збережені в оперативній пам'яті, але які потребують якогось формату обміну або передачі між різними частинами коду або між різними програмами.

2) **Альтернативи:** Альтернативою тимчасовим файлам можуть бути пайпи (в Unix та Linux), анонімні блоки пам'яті або навіть обмін даними через базу даних.

3) **Деталі виконання:** Під час створення тимчасових файлів необхідно враховувати вопроси безпеки. Це означає, що ви повинні забезпечити належний доступ до них, щоб зловмисні процеси не могли використовувати ваші тимчасові файли.

**## Додатково**

[Керівництво по Swift](https://swift.org/documentation/#the-swift-programming-language)

[Принципи роботи з файлами і каталогами в Swift](https://developer.apple.com/documentation/foundation/filemanager)