---
title:                "Читання аргументів командного рядка."
html_title:           "Java: Читання аргументів командного рядка."
simple_title:         "Читання аргументів командного рядка."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви прагнете розбиратися в програмуванні на Java, навичка читання аргументів командного рядка є надзвичайно корисною. Це дозволяє забезпечити більш гнучку та налаштовану програму.

## Як

Для того, щоб читати аргументи командного рядка в Java, використовуйте метод `getArgs()` класу `java.lang.System`. Нижче наведені приклади коду та вихідних даних для демонстрації цього методу.

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        System.out.println("Кількість аргументів командного рядка: " + args.length);
        System.out.println("Аргументи командного рядка:");

        // Цикл для виведення усіх аргументів на екран
        for(String arg: args) {
            System.out.println(arg);
        }
    }
}
```

Вихідні дані, які будуть отримані при запуску цього коду з командного рядка:

`java CommandLineArgs Hello world`

```
Кількість аргументів командного рядка: 2
Аргументи командного рядка:
Hello
world
```

## Глибокий занурення

Метод `getArgs()` повертає масив `String` з усіма переданими аргументами. Якщо не передати жодного аргумента, то масив буде пустий. Нижче наведений приклад з використанням цього методу для отримання аргументів із командного рядка та здійснення потрібних перевірок.

```Java
public static void main(String[] args) {
    // Перевірка на наявність переданих аргументів
    if(args.length == 0) {
        System.out.println("Не вказано жодного аргумента.");
    } else {
        // Отримуємо перший аргумент та виводимо його на екран
        String firstArg = args[0];
        System.out.println("Перший аргумент: " + firstArg);

        // Перевірка на наявність другого аргументу
        if(args.length > 1) {
            // Отримуємо другий аргумент та перевіряємо його на валідність
            int secondArg = Integer.parseInt(args[1]);
            if(secondArg == 0) {
                System.out.println("Другий аргумент не може бути нулем.");
            } else {
                // Використовуємо другий аргумент у обрахунках та виводимо результат на екран
                System.out.println("Результат: " + firstArg.length() / secondArg);
            }
        }
    }
}
```

Вихідні дані при запуску цього коду з командного рядка:

`java CommandLineArgs Java 2`

```
Перший аргумент: Java
Результат: 2
```

`java CommandLineArgs Hello`

```
Перший аргумент: Hello
Не вказано другого аргумента.
```

## Дивись також

- [Java документація про метод getArgs()](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#getArgs--)
- [Стаття про роботу з аргументами командного рядка в Java](https://www.baeldung.com/java-command-line-arguments)