---
title:                "Обробка помилок"
aliases:
- /uk/kotlin/handling-errors.md
date:                  2024-01-26T00:55:12.679829-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обробка помилок"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## Що та Чому?
Обробка помилок - це спосіб, яким ваш код справляється із проблемами, що виникають під час виконання, - ніби ловити крутий м'яч без його втрати. Програмісти це роблять, щоб запобігти збоям і забезпечити користувачам плавний досвід.

## Як це робити:
Kotlin надає `try`, `catch`, `finally` та `throw` для управління помилками. Ось як їх використовувати:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Результат: $result")
    } catch (e: ArithmeticException) {
        println("На нуль ділити не можна, друже.")
    } finally {
        println("Це станеться незалежно від чого.")
    }
}
```

Вивід:
```
На нуль ділити не можна, друже.
Це станеться незалежно від чого.
```

Якщо у блоку `try` виникає щось не так, виконання переходить до `catch`. Він ловить конкретну помилку, що виникла (`ArithmeticException` у цьому випадку). Блок `finally` виконується після цього — незалежно від результату.

## Більш детально
Блок `try-catch` існує з самого початку програмування — це своєрідна сітка безпеки. Kotlin також пропонує `throw` для ручного кидання винятку в код, і є `finally` для коду, який мусить виконатися — це часто завершальна робота.

Альтернативи включають тип `Result` та використання `try` як виразу.

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Цей підхід повертає об'єкт `Result` — ви отримуєте або успіх, або невдачу, без драми некерованого винятку.

Реалізація в Kotlin зручна, тому що ви можете використовувати `try` як вираз, що означає, що воно повертає значення. Такі вибори роблять обробку помилок у Kotlin досить гнучкою. Йдеться про вибір правильного інструменту для завдання, точно так само, як ви б вибрали в майстерні.

## Дивіться також
- Документація Kotlin по Винятках: [Обробка винятків у Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- Документація Kotlin типу `Result`: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- "Ефективне Java, 3-те видання" Джошуа Блоха — гарні інсайти щодо винятків, хоча і специфічно для Java.
