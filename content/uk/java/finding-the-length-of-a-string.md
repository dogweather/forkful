---
title:    "Java: Знаходження довжини рядка"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Існує багато причин, чому можна зацікавитися пошуком довжини рядка у Java. Наприклад, це може бути необхідно для маніпуляцій зі стрічками, форматування виведення або для перевірки коректності введених даних.

## Як

Для знаходження довжини рядка у програмі Java, достатньо використати метод `length()`. Нижче наведено приклад коду для обчислення довжини рядка "Привіт, світ!" та виведення результату:

```Java
String str = "Привіт, світ!";
int length = str.length();
System.out.println("Довжина рядка " + '"' + str + '"' + " дорівнює " + length + ".");
```

Виведення:
```
Довжина рядка "Привіт, світ!" дорівнює 13.
```

## Глибші відомості

Також варто зазначити, що метод `length()` повертає кількість символів у рядку, а не позицію останнього символу. Також він не обчислює пропуски, тому якщо у рядку присутні пробільні символи, вони також будуть враховуватись у довжині рядка.

## Дивіться також

- [Java String API Reference](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java String Tutorial](https://www.w3schools.com/java/java_strings.asp)
- [How to Find String Length in Java](https://www.baeldung.com/java-string-length)