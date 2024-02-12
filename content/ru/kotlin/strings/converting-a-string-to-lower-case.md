---
title:                "Преобразование строки в нижний регистр"
aliases:
- /ru/kotlin/converting-a-string-to-lower-case/
date:                  2024-01-28T23:56:55.384670-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в нижний регистр означает превращение каждого символа в строке в символ нижнего регистра. Программисты делают это для обеспечения согласованности при сравнении, сортировке или сохранении текста.

## Как это сделать:
Функция Kotlin `lowercase()` преобразует все символы строки в нижний регистр быстро. Вот как её использовать:

```kotlin
fun main() {
    val originalString = "ThiS iS A MixED cAsE String!"
    val lowerCaseString = originalString.lowercase()

    println(lowerCaseString) // Вывод: this is a mixed case string!
}
```
Вызовите `lowercase()` и всё готово. Регистр исходных символов не имеет значения; результат всегда в нижнем регистре.

## Погружение в тему
Kotlin не изобрел колесо заново для преобразования строк в нижний регистр. Это действительно общая функция во многих языках программирования. Исторически, функции вроде `tolower()` в C давно решают задачу преобразования регистра.

Теперь, два нюанса при преобразовании в нижний регистр: локали и производительность. Функция `lowercase()` Kotlin может принимать `Locale`, потому что, удивительно, преобразование регистров символов не универсально. Например, турецкие точечные и безточечные "I" ведут себя уникально при преобразовании регистров.

Производительность? В большинстве приложений вы не заметите разницы. Но обработка больших объемов текста требует больше памяти и времени, так как строки в Kotlin неизменяемы. Когда вы приводите строку к нижнему регистру, вы получаете новую строку.

Старожилы помнят `.toLowerCase()` — сейчас Kotlin предпочитает `lowercase()` для ясности.

## См. также
- Документация по строкам Kotlin: [Kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Для обработки текста и продвинутого манипулирования регистрами, смотрите API `java.lang.String`: [Oracle Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- Понимание локалей и особенностей языков: [Oracle Locale Docs](https://docs.oracle.com/javase/tutorial/i18n/locale/)
