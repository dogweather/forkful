---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і для чого?

Видалення символів, що відповідають певному шаблону, це процес заміни цих символів у рядку на інші символи або їх видалення. Програмісти роблять це, щоб маніпулювати рядками і покращувати читабельність даних.

## Як це зробити:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hello, Java!";
        String pattern = "[,!]"; 
        String replaced = str.replaceAll(pattern , "");
        System.out.println(replaced);  
    }
}
```
У вихідних даних, при запуску цього прикладу, буде "Hello Java".

## Поглиблений аналіз:

1. Історичний контекст: Метод заміни в Java був введений ще в JDK 1.2, що було випущено в 1998 році.
2. Альтернативи: Ви також можете використовувати StringBuilder або StringBuffer для видалення символів, що відповідають шаблону, але цей метод менш ефективний по часу виконання.
3. Деталі реалізації: Метод replaceAll використовує регулярні вирази для знаходження шаблонів у рядку. Це може бути некорисним у випадках, коли ваш шаблон складний або видалення його триває довго.

## додаткова інформація:
* [Документація Java для replaceAll](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll(java.lang.String,%20java.lang.String))
* [Java String replaceAll метод](https://www.javatpoint.com/java-string-replaceall)