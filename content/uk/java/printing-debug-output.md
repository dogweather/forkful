---
title:                "Java: Друк відладкового виводу"
simple_title:         "Друк відладкового виводу"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

Програмування - це складний і творчий процес, і часом виникають ситуації, коли наш код не працює так, як очікувалось. Тому важливо знати, як відлагоджувати наш код, і в цьому нам допомагає використання виведення відладочної інформації.

## Як

Існує кілька способів виведення відладочної інформації в Java, але один з найбільш часто використовуваних - це за допомогою методу `System.out.println()`. Давайте розглянемо приклад коду, який виводить рядок "Hello World" в консоль:

```java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello World");
    }
}
```

В результаті виконання цього коду ми отримаємо наступний вивід:

```
Hello World
```

Також, ми можемо використовувати метод `System.out.print()` для виведення відладочної інформації без переводу на новий рядок. Наприклад, якщо змінити приклад коду наступним чином:

```java
public class Main {
    public static void main(String[] args) {
        System.out.print("Hello ");
        System.out.println("World");
    }
}
```

То наш вивід буде виглядати наступним чином:

```
Hello World
```

Це дуже корисний метод при відлагодженні коду, оскільки ми можемо виводити значення змінних і об'єктів, щоб перевірити чи вони містять правильні дані. Наприклад:

```java
int number = 5;
System.out.println("Number value is: " + number);
```

Це допомагає нам переконатися, що значення змінної `number` вірне.

## Deep Dive

Якщо наш код містить баги або не працює так, як очікувалось, ми можемо використати параметр `-ea` при запуску програми для включення перевірки на runtime. Це дозволить бачити спеціальні повідомлення про помилки, які може генерувати Java Virtual Machine. Наприклад:

```java
int number = 10;
assert number > 20 : "Number must be greater than 20";
```

Якщо у нас включений параметр `-ea`, то при запуску програми ми побачимо наступне повідомлення про помилку:

```
Exception in thread "main" java.lang.AssertionError: Number must be greater than 20
	at Main.main(Main.java:3)
```

## See Also

- [How to Debug Your Java Code](https://www.baeldung.com/java-debugging)
- [Debugging with Eclipse](https://www.vogella.com/tutorials/EclipseDebugging/article.html)
- [Java Debugging Tips and Educational Tools](https://www.slidewaysix.com/java-debugging-tips-educational-tools/)