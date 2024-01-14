---
title:                "Java: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Командний рядок - це потужний інструмент, який може полегшити нашу роботу з програмним кодом. Ця стаття допоможе вам зрозуміти, як читати аргументи командного рядка в Java і використовувати їх у ваших проектах.

## Як

Існує кілька способів читати командні аргументи в Java. Розглянемо декілька прикладів коду, щоб ви могли зрозуміти, як це працює.

```java
public static void main(String[] args) {
    // Отримуємо аргументи з командного рядка
    for (String arg : args) {
        // Виводимо аргумент у консоль
        System.out.println(arg);
    }
}
```

Якщо запустити цей код з командного рядка, наприклад `java Main.java arg1 arg2`, то ви побачите `arg1 arg2` виведені у консоль.

Ще один спосіб - використовувати метод `getOpt()` з класу `org.apache.commons.cli`, як показано у наступному прикладі:

```java
// імпортуємо необхідні пакети
import org.apache.commons.cli.*;

public static void main(String[] args) {
    Options options = new Options();

    // додаємо параметр "-f" з необов'язковим значенням для файлу
    options.addOption("f", true, "Файл для обробки");

    String fileName = "default.txt"; // значення за замовчуванням
    CommandLineParser parser = new DefaultParser();
    try {
        // парсимо аргументи командного рядка
        CommandLine cmd = parser.parse(opts, args);

        // отримуємо значення параметра "-f", якщо воно було передано
        if (cmd.hasOption("f")) {
            fileName = cmd.getOptionValue("f");
        }
    } catch (ParseException e) {
        e.printStackTrace();
    }

    // використовуємо fileName у своєму коді
}
```

Не забудьте підключити бібліотеку `commons-cli` до свого проекту.

## Поглиблення

Існує багато різних бібліотек та фреймворків, які допоможуть вам обробляти аргументи командного рядка в Java. Рекомендуємо докладніше ознайомитись з кожним з них, щоб знайти той, який найкраще підходить для вашого проекту.

## Дивіться також

- [Документація по командному рядку Java](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Посібник з обробки аргументів командного рядка в Java](https://www.codejava.net/java-core/how-to-parse-command-line-arguments-in-java)
- [Курс обробки аргументів командного рядка в Java на Coursera](https://www.coursera.org/learn/java-command-line-arguments/)