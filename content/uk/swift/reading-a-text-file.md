---
title:                "Swift: Читання текстового файлу"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому 
Чи коли-небудь ви хотіли прочитати вміст текстового файлу за допомогою Свіфт, але не знали з чого почати? Розуміння того, як працює читання текстових файлів може допомогти вам ефективно працювати з даними та зробити ваш код більш універсальним.

## Як
Відкривши текстовий файл в Свіфт, вам буде необхідно використати клас `FileHandle`, який надає функціональність для читання та запису даних. Спочатку, ви повинні відкрити файл за допомогою методу `init(forReadingFrom: URL)`, передавши шлях до вашого файлу. Далі, використовуючи метод `readData(ofLength: Int)`, ви можете прочитати бажану кількість байтів з файлу. Наприклад:

```Swift
let fileURL = URL(fileURLWithPath: "/path/to/file.txt")
if let fileHandle = try? FileHandle(forReadingFrom: fileURL) {
	let data = fileHandle.readData(ofLength: 10)
	print(String(data: data, encoding: .utf8)) // буфер з першими 10 байтами з файлу (якщо вони існують)
} else {
	print("Не вдалося відкрити файл")
}
```

Важливо пам'ятати закрити файл, викликавши метод `closeFile()` для уникнення проблем з файловою системою вашої операційної системи.

## Глибоке вивчення
Якщо ви бажаєте більш детально ознайомитися з темою читання текстових файлів в Свіфт, ви можете обрати один з наступних варіантів:

- Для початківців: [Робота з файлами у Свіфт](https://www.hackingwithswift.com/read/6/4/reading-from-a-file)
- Поглиблений підхід: [Робота зі збереженням даних у файловій системі](https://www.raywenderlich.com/501-intermediate-swift-tutorial-saving-data-with-file-manager)
- Офіційна документація Свіфт: [Робота з файловими операціями](https://developer.apple.com/documentation/foundation/filemanager)

## Дивіться також
- [Опис класу FileHandle у документації Свіфт](https://developer.apple.com/documentation/foundation/filehandle)
- [Офіційна документація з роботи зі збереженням даних в Свіфт](https://developer.apple.com/documentation/foundation/archives_and_serialization)