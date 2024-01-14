---
title:    "Kotlin: З'єднання рядків"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Чому

Конкатенація - це процес об'єднання двох або більше рядків в один. Це корисно, коли вам потрібно створити складніший рядок зі змінними частинами.

## Як

```Kotlin
val firstName = "Андрій"
val lastName = "Іванченко"
val fullName = firstName + " " + lastName

println(fullName) // виведе: Андрій Іванченко
```

У цьому прикладі ми використовуємо операцію конкатенації `+` для об'єднання двох рядків `firstName` та `lastName` зі знаком пробілу між ними. Результат - рядок `fullName`, який містить повне ім'я.

## Profondeur

Крім операції конкатенації `+`, у Kotlin є ще кілька способів об'єднання рядків. Одним з них є використання функції `plus()`:

```Kotlin
val firstName = "Андрій"
val lastName = "Іванченко"
val fullName = firstName.plus(" ").plus(lastName)

println(fullName) // виведе: Андрій Іванченко
```

Також можна використовувати функцію `plus()` для об'єднання рядків змінної кількості:

```Kotlin
val numbers = listOf("Один", "Два", "Три")
val numbersString = numbers.joinToString(separator = " + ")

println(numbersString) // виведе: Один + Два + Три
```

Крім цього, для об'єднання більшої кількості рядків можна використовувати функцію `concat()`:

```Kotlin
val numbers = arrayOf("Чотири", "П'ять")
val numbersConcat = numbers.concat()

println(numbersConcat) // виведе: ЧотириП'ять
```

У цьому прикладі функція `concat()` об'єднує всі рядки з масиву `numbers` в один рядок.

## Дивись також

- [Офіційна документація Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-concatenation)
- [Підручник з Kotlin для початківців](https://blog.jetbrains.com/kotlin/2020/12/strings-in-kotlin#concatenation)
- [Стаття про рядки у Kotlin](https://www.baeldung.com/kotlin/string-concatenation)