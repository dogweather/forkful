---
title:    "Swift: Написання текстового файлу"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому 

Ця стаття присвячена написанню текстового файлу в мові програмування Swift. Якщо ви хочете зберігати інформацію чи даниі в текстовому форматі, наприклад, список задач, то ця техніка буде корисною для вас.

## Як написати текстовий файл в Swift

Для написання текстового файлу використовується спеціальний клас "FileHandle". Нижче наведено приклад коду та його вихідного результату:

```Swift
let fileName = "todoList.txt"
let fileURL = DocumentManager.getDocumentsDirectory().appendingPathComponent(fileName)

let todoList = """
1. Зробити закупки
2. Забронювати білети на поїздку
3. Підготувати презентацію
"""

do {
   try todoList.write(toFile: fileURL, atomically: true, encoding: .utf8)
   print("Текстовий файл був успішно записано!")
} catch {
    print("Помилка при записуванні файлу: \(error)")
}
```

Результат виконання цього коду буде текстовий файл з назвою "todoList.txt" з наступним вмістом:

```
1. Зробити закупки
2. Забронювати білети на поїздку
3. Підготувати презентацію
```

## Глибока аналітика написання текстових файлів

Тепер, коли ви знаєте як написати текстовий файл в Swift, давайте розглянемо деякі глибші знання про цей процес. Перш за все, важливо знати, що ви можете використовувати різні кодування для збереження текстових файлів, наш приклад використовував `.utf8`. Також, важливо зазначити, що ви можете використовувати і інші методи запису до файлу, наприклад "append" чи "write(contentsOf:)".

## Дивитися також

  - [Офіційна документація Apple про файловий менеджмент в Swift](https://developer.apple.com/documentation/foundation/file_management)
  - [Стаття на Medium про роботу з файлами в Swift](https://medium.com/ios-os-x-development/ios-swift-read-write-text-file-12b4d686b3fa) 
  - [Туторіал з YouTube про роботу зі зчитуванням та записом файлів в Swift](https://www.youtube.com/watch?v=lDk1Sgqbwjg)