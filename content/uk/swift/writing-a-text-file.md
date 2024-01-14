---
title:                "Swift: Написання текстового файлу"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Існує багато причин, чому хтось може зацікавитися написанням текстового файлу в програмуванні Swift. Наприклад, це може бути необхідно для збереження важливих даних, створення звітів або ж для створення конфігураційного файлу для своєї програми. 

## Як це зробити
Для написання текстового файлу в Swift можна використовувати клас `FileManager`, який надає доступ до файлової системи. Ось приклад коду для створення і запису тексту в файл:
```Swift
let text = "Привіт, світ!"
let fileManager = FileManager.default
let fileURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first?.appendingPathComponent("hello.txt")

do {
    try text.write(to: fileURL!, atomically: true, encoding: String.Encoding.utf8)
    print("Текст успішно записано в файл!")
 } catch {
     print("Помилка: \(error)")
 }
```

При виконанні цього коду у вашій папці "Документи" з'явиться файл з назвою "hello.txt" з написаним у ньому текстом "Привіт, світ!".

## Розглиблене вивчення
Є багато цікавих способів для роботи з текстовими файлами в Swift. Наприклад, можна використовувати розширення `Data` для збереження даних у вигляді байтів і потім записувати ці дані в файл. 

Також, є можливість використовувати різні кодировки для запису тексту у файл. Наприклад, для кирилиці можна використовувати кодировку `kCFStringEncodingDOSRussian` у розширенні `CFString` для збереження тексту у файл у кодуванні DOS.

## Дивитися також
- [Документація Apple про клас FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Стаття про роботу з файловою системою в Swift](https://learnappmaking.com/filemanager-files-swift-how-to/)