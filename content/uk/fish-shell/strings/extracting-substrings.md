---
date: 2024-01-20 17:45:32.311112-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: Output."
lastmod: '2024-04-05T21:53:50.094301-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## Як це робити:
```Fish Shell
set str "Файна погода в Україні сьогодні!"
echo $str[7..13] # Видруковує "погода"
echo $str[1..6]  # Видруковує "Файна"
echo $str[-14..-8] # Видруковує "Україні"
```
Output:
```
погода
Файна
Україні
```

## Підводні камені
Раніше, у старих оболонках, витягнути підрядок було більш складно. Fish Shell спростив процес завдяки використанню квадратних дужок для індексації. Але не забуваймо про `string` команду для більш складних завдань. Bash з його `${string:position:length}` підходом виглядає більш громіздко на фоні Fish. Щодо реалізації, Fish використовує нульову індексацію і дозволяє від'ємні індекси для роботи з кінця рядка.

## Дивіться також:
- [Fish Documentation on String Manipulation](https://fishshell.com/docs/current/cmds/string.html)
- [Stack Overflow discussions on Fish](https://stackoverflow.com/questions/tagged/fish)
- [Learn X in Y minutes - Fish](https://learnxinyminutes.com/docs/fish/)
