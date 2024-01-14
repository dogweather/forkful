---
title:    "Kotlin: Видобування підстрок"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Чому

Виділення підрядка - це важливий аспект програмування, оскільки дозволяє ефективно працювати зі строками даних. Для українського читача це особливо корисно, оскільки більшість наших данних записані українською мовою.

## Як це зробити?

```Kotlin
var data = "Привіт, це український текст."
var substring = data.substring(6, 12)
println(substring)
```

Виведе: "це український".

```Kotlin
var email = "example@email.com"
var domain = email.substringAfter("@")
println(domain)
```

Виведе: "email.com".

## Розбір деталей

В Kotlin, метод `substring()` приймає два параметри - початковий і кінцевий індекс підрядка, який потрібно виділити. Ще один корисний метод - `substringAfter()` - повертає підрядок після заданого роздільника. Це дуже зручно для роботи зі строками, які містять спеціальні символи або формати, наприклад електронна пошта або URL.

## Дивіться також

- [Kotlin Strings](https://kotlinlang.org/docs/reference/strings.html)
- [Java Substring](https://www.w3schools.com/java/ref_string_substring.asp)
- [Regular Expressions in Kotlin](https://kotlinlang.org/docs/reference/regular-expressions.html)