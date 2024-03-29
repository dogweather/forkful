---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:31.298470-07:00
description: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432\
  \ Kotlin \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0441\u0442\
  \u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0444\u0430\u0439\u043B\u0443 \u0442\
  \u0430 \u0432\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0432\u043C\u0456\u0441\u0442\u0443 \u0432\
  \ \u043D\u044C\u043E\u0433\u043E, \u0449\u043E \u0454 \u043F\u043E\u0448\u0438\u0440\
  \u0435\u043D\u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F\u043C\
  \ \u0434\u043B\u044F \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F\
  \ \u0434\u0430\u043D\u0438\u0445,\u2026"
lastmod: '2024-03-13T22:44:49.249879-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432\
  \ Kotlin \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0441\u0442\
  \u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0444\u0430\u0439\u043B\u0443 \u0442\
  \u0430 \u0432\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0432\u043C\u0456\u0441\u0442\u0443 \u0432\
  \ \u043D\u044C\u043E\u0433\u043E, \u0449\u043E \u0454 \u043F\u043E\u0448\u0438\u0440\
  \u0435\u043D\u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F\u043C\
  \ \u0434\u043B\u044F \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F\
  \ \u0434\u0430\u043D\u0438\u0445,\u2026"
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
---

{{< edit_this_page >}}

## Що і чому?
Створення текстового файлу в Kotlin передбачає створення файлу та введення текстового вмісту в нього, що є поширеним завданням для зберігання даних, логування або налаштувань конфігурації. Програмісти роблять це, щоб зберегти та маніпулювати даними за межами волатильної пам'яті, забезпечуючи постійність між сесіями.

## Як це зробити:
Kotlin пропонує простий підхід для запису в файли, використовуючи стандартну бібліотеку без необхідності додавання сторонніх бібліотек. Ось простий приклад:

```kotlin
import java.io.File

fun main() {
    val textToWrite = "Привіт, запис файлу на Kotlin!"
    File("example.txt").writeText(textToWrite)
}
```
Цей фрагмент коду створює файл з назвою "example.txt" у кореневій директорії проекту та записує рядок `Привіт, запис файлу на Kotlin!` в нього. Якщо файл вже існує, він буде перезаписаний.

Для більш контрольованого додавання до файлу або запису більших обсягів даних, ви можете використовувати `appendText` або `bufferedWriter()`:

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "Додаємо ще текст."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "Великі обсяги тексту...\nНа кількох рядках."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // Додає текст до існуючого файлу
    writeWithBufferedWriter() // Записує великі обсяги текстових даних ефективно
}
```

У функції `appendToFile` ми додаємо більше тексту до "example.txt", не перезаписуючи його поточний вміст. Функція `writeWithBufferedWriter` демонструє ефективний спосіб запису великих обсягів тексту чи даних, особливо корисний для мінімізації операцій введення/виведення при роботі з кількома рядками чи великими файлами.

Ці приклади охоплюють базові операції для запису текстових файлів у Kotlin, демонструючи простоту та потужність стандартної бібліотеки Kotlin для операцій з файлами введення/виведення.
