---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що таке і навіщо?

Считування текстового файлу - це процес вилучення інформації з такого файлу. Це робить програміст, коли потрібно доступитися до збережених даних або обробляти їх.

## Як це зробити:

Ось як прочитати текстовий файл в Kotlin. Створимо файл `example.txt` і запишемо в нього деякі довідкові дані.

```Kotlin
import java.io.File

fun main() {
    val fileName = "example.txt"
    val lines: List<String> = File(fileName).readLines()
    lines.forEach { line -> println(line) }
}
```
При виконанні цієї програми, виведення буде таким:
```
line1
line2
line3
```
## Пірнемо глибше:

1. **Історичний контекст**: Стандартні бібліотеки Kotlin основано на Java, тому методи зчитування файлів також були запозичені відтуди.
2. **Альтернативи**: Можна використовувати `BufferedReader` або `InputStreamReader`, якщо вам потрібно більше контролю над процесом зчитування.
3. **Деталі реалізації**: Метод `readLines()` зчитує всі рядки з файлу одразу. Якщо файл великий, це може стати проблемою. В таких випадках використовуйте метод `File(fileName).forEachLine`.

## Див ще:

1. Офіційну документацію (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-lines.html)
2. Kotlin File Reading Tuorial (https://www.baeldung.com/kotlin-read-file)
3. Java Input/Output: Reading a Text File tutorial (https://www.codejava.net/java-se/file-io/java-file-tutorial-and-reference)