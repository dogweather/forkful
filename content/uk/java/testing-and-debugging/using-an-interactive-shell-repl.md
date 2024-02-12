---
title:                "Використання інтерактивної оболонки (REPL)"
aliases:
- /uk/java/using-an-interactive-shell-repl/
date:                  2024-01-26T04:15:46.862344-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання інтерактивної оболонки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Що та Чому?
REPL (цикл "читати-виконувати-друкувати") - це інтерактивна оболонка, яка обробляє окремі введення користувача, виконує код і повертає результат. Програмісти використовують її для швидких експериментів, налагодження або навчання, оскільки вона дозволяє отримувати негайний зворотний зв'язок і ітерацію.

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
