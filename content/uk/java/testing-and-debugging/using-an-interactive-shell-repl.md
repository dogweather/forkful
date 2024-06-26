---
date: 2024-01-26 04:15:46.862344-07:00
description: "\u042F\u043A: \u041F\u043E\u0447\u0430\u0442\u0438 \u0440\u043E\u0431\
  \u043E\u0442\u0443 \u0432 REPL \u0432 Java \u043B\u0435\u0433\u043A\u043E \u0437\
  \u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0456\u043D\u0441\
  \u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0443 `jshell`, \u043F\u0440\u0435\u0434\
  \u0441\u0442\u0430\u0432\u043B\u0435\u043D\u043E\u0433\u043E \u0432 Java 9. \u041E\
  \u0441\u044C \u044F\u043A \u0446\u0435 \u043C\u043E\u0436\u043D\u0430 \u0437\u0440\
  \u043E\u0431\u0438\u0442\u0438 \u0442\u0430 \u0440\u043E\u0437\u043F\u043E\u0447\
  \u0430\u0442\u0438 \u0431\u0430\u0437\u043E\u0432\u0443 \u0441\u0435\u0441\u0456\
  \u044E."
lastmod: '2024-03-13T22:44:49.081757-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u0447\u0430\u0442\u0438 \u0440\u043E\u0431\u043E\u0442\u0443\
  \ \u0432 REPL \u0432 Java \u043B\u0435\u0433\u043A\u043E \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0456\u043D\u0441\u0442\u0440\u0443\u043C\
  \u0435\u043D\u0442\u0443 `jshell`, \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\
  \u043B\u0435\u043D\u043E\u0433\u043E \u0432 Java 9."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0456\
  \u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0457 \u043E\u0431\
  \u043E\u043B\u043E\u043D\u043A\u0438 (REPL)"
weight: 34
---

## Як:
Почати роботу в REPL в Java легко за допомогою інструменту `jshell`, представленого в Java 9. Ось як це можна зробити та розпочати базову сесію:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  створено метод sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Вийти в будь-який час за допомогою `/exit`.

```Java
jshell> /exit
|  До побачення
```

## Поглиблений огляд
До `jshell`, програмісти Java не мали офіційного REPL, на відміну від розробників Python або Ruby. Вони використовували IDE або писали повні програми навіть для тривіальних завдань. `jshell` стало змінною грою з Java 9, заповнивши цю прогалину.

Альтернативи включають онлайн компілятори або плагіни для IDE, але вони не дорівнюють негайності `jshell`. Щодо внутрішнього використання, `jshell` використовує Java Compiler API для виконання фрагментів коду, що дуже зручно. Це більше, ніж майданчик для ігор — він може імпортувати бібліотеки, визначати класи та багато іншого. Це робить його потужним інструментом для прототипування.

## Див. також
- [Керівництво користувача JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Довідкові матеріали зі стандартних інструментів Java Edition](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [API Java Compiler](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
