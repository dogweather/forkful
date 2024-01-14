---
title:                "Java: Перетворення рядка в нижній регістр"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Конвертація рядка в нижній регістр є важливою задачею в програмуванні, оскільки це дозволяє стандартизувати дані та полегшує їх подальшу обробку.

## Як

Конвертація рядка в нижній регістр може бути здійснена за допомогою методу `toLowerCase()` класу `String`. Нижче наведений приклад коду та його вихідний результат:

```Java
String message = "Hello World!";
String convertedMessage = message.toLowerCase();
System.out.println(convertedMessage); // виведе "hello world!" на екран
```

Цей метод також може бути використаний для конвертації окремих символів, наприклад:

```Java
char letter = 'A';
char convertedLetter = Character.toLowerCase(letter);
System.out.println(convertedLetter); // виведе "a" на екран
```

## Глибоке погруження

При виконанні конвертації рядка в нижній регістр, важливо пам'ятати, що цей процес залежить від локалі комп'ютера, який виконує програму. Це означає, що для різних мов та алфавітів можуть бути використані різні символи. Також варто враховувати вплив методу на рядки, що містять цифри та спеціальні символи.

## Дивись також

- [Документація з методу toLowerCase()](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Стаття про кодування символів в Java](https://www.baeldung.com/java-char-encoding)