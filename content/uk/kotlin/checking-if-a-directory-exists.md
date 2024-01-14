---
title:                "Kotlin: Перевірка наявності каталогу"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливою для ефективної роботи з файлами в вашому Kotlin програмуванні. Це може допомогти уникнути непотрібних помилок та забезпечити правильне виконання операцій з файлами.

## Як це зробити

Для перевірки існування директорії у Kotlin використовується функція `exists()` з класу `File`. Приклад реалізації такої перевірки виглядає наступним чином:

```Kotlin
fun checkDirectory(directoryPath: String){
    val folder = File(directoryPath)
    if (folder.exists()){
        println("Директорія існує")
    } else{
        println("Директорія не існує")
    }
}

fun main() {
    checkDirectory("Робочий стіл")
}
```

Приклад виводу:

```
Директорія не існує
```

## Глибокий занурення

Реалізація перевірки існування директорії базується на тому, що вона перевіряє, чи існує файл з вказаним шляхом. Якщо файл не знайдено, то повертається `false`. Таким чином, для перевірки існування директорії використовується фактично функція перевірки файлу.

## Дивись також

- [Документація з перевірки існування директорії в Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- [Порівняння різних методів перевірки існування директорії в Java і Kotlin](https://stackoverflow.com/questions/18166780/how-do-i-check-if-a-directory-exists-in-kotlin-java)
- [Приклади роботи з директоріями в Kotlin](https://www.programiz.com/kotlin-programming/directory)