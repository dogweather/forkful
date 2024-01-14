---
title:    "Java: Перетворення рядка в нижній регістр"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Чому

При перетворенні рядка на знижений регістр, дана функція дозволяє змінити усі великі літери у рядку на малі. Це може бути корисно при знаходженні та порівнянні рядків.

## Як це зробити

Приведення рядка до нижнього регістру можна виконати за допомогою функції `toLowerCase()`.

```Java
String str = "HELLO WORLD";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr);
```
Output: hello world

## Глибоке дослідження

Функція `toLowerCase()` використовує стандарт Unicode для перетворення великих літер на малі. Вона не впливає на символи пунктуації та спеціальні символи, а також не змінює літери інших мов, які не мають відображення у нижньому регістрі.

## Дивитися також

- [Методи рядків у Java](https://www.tutorialspoint.com/java/java_string_methods.htm)
- [Підтримка Unicode у Java](https://www.geeksforgeeks.org/support-for-unicode-support-strings-in-java/)
- [Переваги використання рядків у нижньому регістрі](https://www.baeldung.com/java-string-capitalization)