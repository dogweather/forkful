---
title:    "Kotlin: Написання текстового файлу"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Для чого
Написання текстового файлу є необхідною складовою розробки будь-якої програми. Це дає можливість зберегти дані та передавати їх для подальшої обробки іншим програмам.

## Як
Написання текстового файлу в Kotlin нескладне і виконується за допомогою вбудованих функцій. Ось приклад коду, який демонструє як створити текстовий файл зі строковими даними:

```Kotlin
val text = "Це приклад тексту для запису в файл"
File("example.txt").writeText(text)
```

Цей код створить файл з назвою "example.txt" та збереже в ньому наш текст. Для додавання додаткових даних до файлу, можна використати функцію "appendText()" замість "writeText()".

## Глибший розбір
Коли ми записуємо текстовий файл, ми зазвичай хочемо мати можливість читати ці дані пізніше. Це можна зробити за допомогою функцій "readText()" та "readLines()". Перша функція поверне весь текст файлу у вигляді строки, а друга - розділить цього рядка на список строк.

```Kotlin
val textFromFile = File("example.txt").readText()
val lines = File("example.txt").readLines()
```

При необхідності, можна також використовувати функції "bufferedReader()" та "bufferedWriter()" для більш ефективної роботи з файлом.

## Дивись також
- [Офіційна документація Kotlin](https://kotlinlang.org/docs/reference/basic-input-output.html)
- [Стаття про роботу з файлами в Kotlin](https://www.baeldung.com/kotlin-write-to-file)
- [Відеоурок з написання файлу в Kotlin](https://www.youtube.com/watch?v=f1HsKz523Zo)