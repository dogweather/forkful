---
title:                "Kotlin: Перевірка існування каталогу"
simple_title:         "Перевірка існування каталогу"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Чи ви коли-небудь вступали у ситуацію, коли ваша програма несподівано зупинялася через те, що вона не змогла знайти певну директорію? Якщо так, то ви знаєте, наскільки це є важливим, щоб переконатися, що директорія існує перед тим, як продовжувати виконання програми. У цій статті ми розглянемо, як перевірити, чи існує директорія в коді Kotlin.

## Як

Для перевірки існування директорії в Kotlin ми використаємо функцію `exists()` з класу `File`. Наприклад, якщо нам потрібно перевірити існування директорії з назвою `myFolder` в поточній директорії, ми можемо тут же виконати код Kotlin.

```Kotlin
if (File("myFolder").exists()) {
    println("Директорія існує.")
} else {
    println("Директорія не існує.")
}
```

Якщо директорія `myFolder` існує, ви побачите повідомлення `Директорія існує.` у консолі. В іншому випадку ви побачите повідомлення `Директорія не існує.`.

## Глибинний аналіз

Існує кілька більш глибоких аспектів перевірки існування директорії, які можуть бути корисними для вас. Наприклад, ви можете використовувати функцію `isDirectory` для перевірки, чи є цей об'єкт `File` директорією. Також, якщо ви хочете переконатися, що ця директорія єдина в своєму роді, ви можете використовувати функцію `isUnique()`.

## Дивіться також

- [Документація Kotlin: Class File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Стаття на Medium: Checking if a Directory Exists in Kotlin](https://medium.com/@krispypen/checking-if-a-directory-exists-in-kotlin-c5243b884821)
- [Відео на YouTube: Checking if a Directory Exists in Kotlin](https://www.youtube.com/watch?v=B6x9pDBSAoY)