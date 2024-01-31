---
title:                "Виділення підрядків"
date:                  2024-01-20T17:45:32.311112-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Витягування підрядків дозволяє виділити частину тексту з більшого рядка. Програмісти це роблять, аби обробити або перевірити конкретні дані.

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
