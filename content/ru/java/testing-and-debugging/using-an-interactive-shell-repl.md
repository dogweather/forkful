---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:00.624948-07:00
description: "REPL (Read-Eval-Print Loop \u2014 \u0426\u0438\u043A\u043B \xAB\u0427\
  \u0438\u0442\u0430\u0442\u044C-\u0412\u044B\u043F\u043E\u043B\u043D\u044F\u0442\u044C\
  -\u0412\u044B\u0432\u043E\u0434\u0438\u0442\u044C\xBB) \u2014 \u044D\u0442\u043E\
  \ \u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u0430\u044F\
  \ \u043E\u0431\u043E\u043B\u043E\u0447\u043A\u0430, \u043A\u043E\u0442\u043E\u0440\
  \u0430\u044F \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0435\u0442\
  \ \u043E\u0442\u0434\u0435\u043B\u044C\u043D\u044B\u0435 \u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044C\u0441\u043A\u0438\u0435 \u0432\u0445\
  \u043E\u0434\u043D\u044B\u0435\u2026"
lastmod: '2024-03-13T22:44:44.823630-06:00'
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print Loop \u2014 \u0426\u0438\u043A\u043B \xAB\u0427\u0438\
  \u0442\u0430\u0442\u044C-\u0412\u044B\u043F\u043E\u043B\u043D\u044F\u0442\u044C\
  -\u0412\u044B\u0432\u043E\u0434\u0438\u0442\u044C\xBB) \u2014 \u044D\u0442\u043E\
  \ \u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u0430\u044F\
  \ \u043E\u0431\u043E\u043B\u043E\u0447\u043A\u0430, \u043A\u043E\u0442\u043E\u0440\
  \u0430\u044F \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0435\u0442\
  \ \u043E\u0442\u0434\u0435\u043B\u044C\u043D\u044B\u0435 \u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044C\u0441\u043A\u0438\u0435 \u0432\u0445\
  \u043E\u0434\u043D\u044B\u0435\u2026"
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0439\
  \ \u043E\u0431\u043E\u043B\u043E\u0447\u043A\u0438 (REPL)"
---

{{< edit_this_page >}}

## Что и Почему?
REPL (Read-Eval-Print Loop — Цикл «Читать-Выполнять-Выводить») — это интерактивная оболочка, которая обрабатывает отдельные пользовательские входные данные, выполняет код и возвращает результат. Программисты используют её для быстрых экспериментов, отладки или обучения, так как она позволяет получать немедленную обратную связь и итерацию.

## Как использовать:
Запуск REPL в Java прост с помощью инструмента `jshell`, представленного в Java 9. Вот как начать работу и открыть базовую сессию:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  создан метод sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Выйти можно в любое время с помощью команды `/exit`.

```Java
jshell> /exit
|  До свидания
```

## Подробнее
До `jshell`, у программистов Java не было официального REPL, в отличие от разработчиков Python или Ruby. Они использовали среды разработки или писали полные программы даже для тривиальных задач. `jshell` стал переломным моментом начиная с Java 9, преодолев этот пробел.

Альтернативами являются онлайн-компиляторы или плагины для IDE, но они не соответствуют немедленности `jshell`. Что касается внутреннего устройства, `jshell` использует Java Compiler API для выполнения фрагментов кода, что довольно изящно. Это больше, чем просто песочница — он может импортировать библиотеки, определять классы и многое другое. Это делает его мощным инструментом для создания прототипов.

## Смотрите также
- [Руководство пользователя JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Справочник по инструментам Java Platform, Standard Edition](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
