---
title:    "Java: З'єднання рядків"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Створення, збереження та обробка інформації - це основна частина багатьох програм, що розробляються на мові програмування Java. Для збереження та подальшої обробки даних, часто потрібно з'єднувати різні рядки тексту. У самому Java є спеціальний механізм для цього - конкатенація рядків.

## Як використовувати конкатенацію рядків

Для конкатенації рядків в Java можна використовувати оператор "+" або метод "concat()". Нижче наведені приклади коду та відповіді, що можуть бути виведені в консоль:

```java
// Застосування оператора "+"
String name = "Марія";
String greeting = "Привіт ";
System.out.println(greeting + name);

// Виведе: Привіт Марія

// Застосування методу "concat()"
String str1 = "Hello";
String str2 = "World";
String str3 = str1.concat(str2);
System.out.println(str3);

// Виведе: HelloWorld
```

Конкатенація рядків також можлива з використанням методу "StringBuilder", який забезпечує більш ефективне з'єднання багатьох рядків.

## Глибше занурення

У Java конкатенація рядків відбувається шляхом створення нового об'єкта String. При використанні оператора "+" компілятор автоматично створює об'єкт StringBuilder, що дозволяє ефективно додавати до нього рядки. При використанні методу "concat()" змінні значення об'єктів не змінюються, а створюється новий об'єкт зєднаних рядків.

## Дивись також

- [Документація Java String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Туторіал по конкатенації рядків](https://www.programiz.com/java-programming/string-concatenation)
- [Методи StringBuilder у Java](https://www.javatpoint.com/java-stringbuilder)