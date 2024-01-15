---
title:                "Читання текстового файлу."
html_title:           "Swift: Читання текстового файлу."
simple_title:         "Читання текстового файлу."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Для того, щоб прочитати текстовий файл, необхідний початковий рівень знань Swift. Це дозволить вам здійснювати операції з файлами, наприклад, читати та записувати дані, що є корисним для ведення журналів або завантаження вмісту з Інтернету. Це також допомагає у зрозумінні базових концепцій програмування.

## Як це зробити

```Swift
let fileURL = URL(fileURLWithPath: "file.txt") // створюємо посилання на файл
var fileContents = "" // змінна, яка буде містити зчитаний текст
do {
    // використовуємо метод String(contentsOf: URL) для читання даних з файлу
    fileContents = try String(contentsOf: fileURL)
} catch {
    // обробка помилки, якщо файл не був знайдений або не може бути прочитаний
    print("Не вдалося прочитати файл: \(error)")
}
print(fileContents) // виводимо вміст файлу на екран
```
Вивід: Зміст файлу "file.txt".

## Глибше занурення

Як було показано в попередньому прикладі, для читання текстових файлів використовуються об'єкти URL та метод String(contentsOf: URL). Будьте обережні при роботі з файловою системою, оскільки неправильні дії можуть призвести до видалення або пошкодження файлів. Також варто навчитися обробляти помилки, які можуть виникати при читанні файлів, щоб уникнути непередбачуваних помилок у вашій програмі.

## Дивіться також

- [Робота з URL в Swift](https://developer.apple.com/documentation/foundation/url)
- [Керування файлами та директоріями в Swift](https://www.hackingwithswift.com/example-code/system/how-to-work-with-files-and-directories-in-swift)