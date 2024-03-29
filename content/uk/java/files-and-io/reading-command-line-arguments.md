---
date: 2024-01-20 17:56:28.450287-07:00
description: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\
  \u0435\u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\
  \u043E \u0440\u044F\u0434\u043A\u0430 \u0434\u043E\u0437\u0432\u043E\u043B\u044F\
  \u0454 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0447\u0430\u043C\
  \ \u043F\u0435\u0440\u0435\u0434\u0430\u0432\u0430\u0442\u0438 \u043F\u0430\u0440\
  \u0430\u043C\u0435\u0442\u0440\u0438 \u0443 \u0432\u0430\u0448\u0443 Java \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0443 \u043F\u0456\u0434 \u0447\u0430\u0441\
  \ \u0457\u0457 \u0437\u0430\u043F\u0443\u0441\u043A\u0443. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.105586-06:00'
model: gpt-4-1106-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\
  \u0435\u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\
  \u043E \u0440\u044F\u0434\u043A\u0430 \u0434\u043E\u0437\u0432\u043E\u043B\u044F\
  \u0454 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0447\u0430\u043C\
  \ \u043F\u0435\u0440\u0435\u0434\u0430\u0432\u0430\u0442\u0438 \u043F\u0430\u0440\
  \u0430\u043C\u0435\u0442\u0440\u0438 \u0443 \u0432\u0430\u0448\u0443 Java \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0443 \u043F\u0456\u0434 \u0447\u0430\u0441\
  \ \u0457\u0457 \u0437\u0430\u043F\u0443\u0441\u043A\u0443. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\u2026"
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430"
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
