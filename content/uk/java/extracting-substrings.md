---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Шо & Навіщо?

Видобування підрядків (substrings) - це процес вибору певної частини стрічки за індексами. Програмісти це роблять, коли потрібно обробити частину даних в рамках більшої стрічки.

## Як це зробити:

Метод `substring()` в Java використовується для видобування підрядків. Спробуйте подивитися на цей приклад:

```Java
public class Main {
  public static void main(String[] args) {
    String str = "Життя - це подорож, а не мета.";
    String subStr = str.substring(8, 16);
    System.out.println(subStr);
  }
}
```

Виведуться слова "це подорож". Оголошено стрічку `str`, потім вибрано підрядок з 8 по 16 індекси. Цей підрядок виведено на екран.

## Поглиблений матеріал

Метод `substring()` був введений у Java 1.0, що робить його одним з найстаріших методів мови для роботи зі стрічками. Альтернативою може бути використання методу `split()`, який розбиває стрічку на масив підрядків за вказаним символом.

Часто на практиці програмісти використовують `substring()` разом з `indexOf()`, щоб визначити, звідки починати видобування підрядка.

## Дивіться також

1. [Java String substring() Method - W3Schools](https://www.w3schools.com/java/ref_string_substring.asp)
2. [Extract a substring in Java - Stack Overflow](https://stackoverflow.com/questions/16027229/extracting-a-substring-in-java)
3. [Java String substring() method with examples - TutorialsPoint](https://www.tutorialspoint.com/java/java_string_substring.htm)