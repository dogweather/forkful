---
title:                "Использование интерактивной оболочки (REPL)"
aliases:
- /ru/java/using-an-interactive-shell-repl/
date:                  2024-01-29T00:04:00.624948-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование интерактивной оболочки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
