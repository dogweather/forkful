---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання текстового файлу - це процес вилучення даних з файлу, збереженого у текстовому форматі. Програмісти це роблять, щоб обробляти і маніпулювати інформацією, яка міститься в цих файлах у вигляді тексту.

## Як це зробити:
Ось базовий приклад, як можна прочитати текстовий файл в Swift:
```swift
import Foundation

let path = "/path/to/your/textFile.txt"

do {
    let text = try String(contentsOfFile: path, encoding: .utf8)
    print(text)
} 
catch {
    print("Couldn't read file.")
}
```
Вихідні дані будуть виглядати так:
```
Your text file content here
```

## Занурення у деталі
Історично, прочитати текстовий файл було важливим завданням ще з часів, коли комп'ютери першого покоління мали лімітовану пам'ять. Програмісти шукали способи ефективного зберігання даних.

Альтернативою читанню файлу через описаний вище метод може бути використання потоків і буферизований ввод/вивід.

Фактично, коли ви читаєте текстовий файл в Swift, відбувається кілька внутрішніх процесів: ваша програма відкриває файл, читає його вміст блоками, обробляє цей вміст, а потім закриває файл.

## Дивіться також:
- Документація Apple про роботу з файлами: https://developer.apple.com/documentation/foundation/filemanager
- Туторіал на YouTube про читання/письмо в файл: https://www.youtube.com/watch?v=EAGz-RNRilA.