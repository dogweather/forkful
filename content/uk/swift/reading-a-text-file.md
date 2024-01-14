---
title:                "Swift: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

У цій статті ми розглянемо те, як читати текстовий файл за допомогою Swift. Якщо ви хочете дізнатися, як зчитувати дані з текстового файлу, ця стаття для вас!

## Як

Щоб зчитати текстовий файл за допомогою Swift, нам потрібно використовувати клас "FileManager". Спочатку нам потрібно отримати шлях до файлу:

```Swift
let path = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent("file.txt")
```

Потім ми можемо створити змінну типу "String", в яку будемо зберігати всі дані з файлу:

```Swift
var fileContent: String = ""
```

Тепер ми можемо використати метод "contents(atPath: String)", щоб отримати дані з нашого файлу:

```Swift
if let data = FileManager.default.contents(atPath: path.path) {
    fileContent = String(data: data, encoding: .utf8) ?? ""
}
```

Цей метод повертає дані у вигляді "Data", тому нам потрібно перетворити їх у "String". Тепер ми можемо вивести отримані дані на екран:

```Swift
print("Зміст файлу:", fileContent)
```

Якщо наш файл містить наступний текст:

```
Привіт у всіх!
Світ Swift чекає!
```

Ми отримаємо наступний вихід:

```
Зміст файлу: Привіт у всіх!
Світ Swift чекає!
```

## Deep Dive

Файли можуть містити багато різноманітної інформації, тому саме для цього Swift має багато функціональних можливостей для читання даних. Наприклад, якщо файл повертає дані у форматі "Data", ми можемо використати функції "JSONSerialization" для розшифрування цих даних у зрозумілий формат.

Також можна використовувати різні методи для отримання конкретної інформації з файлу, наприклад, читати по рядках, визначати розмір файлу та багато іншого.

## Дивитися також

1. [Робота з файлами та каталогами в Swift](https://www.raywenderlich.com/7045-working-with-files-and-directories-in-swift)
2. [Робота з JSON в Swift](https://medium.com/kinstate/how-to-parse-json-file-in-swift-5a37c25ba1f1)
3. [Документація Apple про клас FileManager](https://developer.apple.com/documentation/foundation/filemanager)