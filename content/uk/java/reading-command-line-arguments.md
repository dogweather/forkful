---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:56:28.450287-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Читання аргументів командного рядка дозволяє користувачам передавати параметри у вашу Java програму під час її запуску. Програмісти використовують це для забезпечення гнучкості та конфігурації відповідно до потреб користувача, не змінюючи код.

## Як це зробити:
```java
public class CommandLineArgs {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Аргументи командного рядка:");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("Аргументи не були передані.");
        }
    }
}
```
**Вивід при запуску:** `java CommandLineArgs Привіт Україно!`
```
Аргументи командного рядка:
Привіт
Україно!
```

## Поглиблений розбір
Читання аргументів командного рядка - дуже стара практика, що йшла від мов UN*X шелл скріптів. Існують альтернативи, як наприклад, зчитування даних з файлу конфігурації або змінні оточення, але аргументи командного рядка залишаються популярними через свою прямоту та зручність у скриптах та автоматизації. У Java, ці параметри представлені у вигляді масиву рядків (`String[] args`), що передається вашому методу `main`. Якщо параметри не передані, масив буде порожнім, але не `null`.

## Додаткові ресурси
- [Java Command Line Arguments tutorial](https://www.javatpoint.com/command-line-argument)
- [Oracle Java documentation](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Understanding Java Main Method](https://www.baeldung.com/java-main-method)
