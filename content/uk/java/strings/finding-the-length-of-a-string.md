---
title:                "Визначення довжини рядка"
aliases: - /uk/java/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:42.015827-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Визначення довжини рядка в Java – це про визначення кількості символів у рядку. Це робиться, щоб керувати текстовими даними, здійснювати перевірки, обрізки, чи в цілому, робити програми отой фігні не піддаються.

## Як це зробити:
В Java, `.length()` - це те, що тобі треба. Мінімалістичний. Диви:
```java
public class StringLengthExample {
    public static void main(String[] args) {
        String hello = "Привіт";
        System.out.println("Рядок: " + hello + " | Довжина: " + hello.length());
        
        String empty = "";
        System.out.println("Рядок: " + empty + " | Довжина: " + empty.length());
        
        String spaces = "   ";
        System.out.println("Рядок: " + spaces + " | Довжина: " + spaces.length());
    }
}
```
Вивід буде:
```
Рядок: Привіт | Довжина: 6
Рядок:  | Довжина: 0
Рядок:    | Довжина: 3
```
Просто і до теми. Далі поїхали.

## Більше Знань:
Раніше в історії Java ми вже мали цю `.length()` функцію - вона тут від самого початку. Альтернативи? Ну, не дуже... Є різниця між `.length()` для рядків та `.length` для масивів, але для рядків, хлопці, то це ваш інструмент. Щодо деталей реалізації, `.length()` рахує Unicode символи як одиниці, навіть якщо символ – це всмислі два кодові пункти. А якщо хочеш покопатись в UTF-16, братан, тримай знаття про `.codePointCount()`, але на повседневній основі, `.length()` кроє все, що треба зрізати.

## Дивись Також:
- [Java String документація](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html) – довгий текст, але все про рядки.
- [Unicode-конспект](https://unicode-table.com/en/) – щоб розуміти суть символів, з якими ти працюєш.

Роби своє, не зациклюйся, і вуаля - отримуєш саме те, що треба.
