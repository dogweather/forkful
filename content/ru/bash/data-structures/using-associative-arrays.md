---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:10.452294-07:00
description: "\u0410\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\
  \u0435 \u043C\u0430\u0441\u0441\u0438\u0432\u044B \u044F\u0432\u043B\u044F\u044E\
  \u0442\u0441\u044F \u0441\u0432\u043E\u0435\u0433\u043E \u0440\u043E\u0434\u0430\
  \ \u0443\u0441\u0438\u043B\u0435\u043D\u043D\u044B\u043C\u0438 \u043C\u0430\u0441\
  \u0441\u0438\u0432\u0430\u043C\u0438, \u043A\u043E\u0442\u043E\u0440\u044B\u0435\
  \ \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0442 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0438\
  \ \u0432 \u043A\u0430\u0447\u0435\u0441\u0442\u0432\u0435 \u0438\u043D\u0434\u0435\
  \u043A\u0441\u043E\u0432, \u0430 \u043D\u0435 \u0442\u043E\u043B\u044C\u043A\u043E\
  \ \u0446\u0435\u043B\u044B\u0435 \u0447\u0438\u0441\u043B\u0430.\u2026"
lastmod: '2024-03-13T22:44:45.352521-06:00'
model: gpt-4-0125-preview
summary: "\u0410\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\
  \u0435 \u043C\u0430\u0441\u0441\u0438\u0432\u044B \u044F\u0432\u043B\u044F\u044E\
  \u0442\u0441\u044F \u0441\u0432\u043E\u0435\u0433\u043E \u0440\u043E\u0434\u0430\
  \ \u0443\u0441\u0438\u043B\u0435\u043D\u043D\u044B\u043C\u0438 \u043C\u0430\u0441\
  \u0441\u0438\u0432\u0430\u043C\u0438, \u043A\u043E\u0442\u043E\u0440\u044B\u0435\
  \ \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0442 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0438\
  \ \u0432 \u043A\u0430\u0447\u0435\u0441\u0442\u0432\u0435 \u0438\u043D\u0434\u0435\
  \u043A\u0441\u043E\u0432, \u0430 \u043D\u0435 \u0442\u043E\u043B\u044C\u043A\u043E\
  \ \u0446\u0435\u043B\u044B\u0435 \u0447\u0438\u0441\u043B\u0430.\u2026"
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0430\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\u0445\
  \ \u043C\u0430\u0441\u0441\u0438\u0432\u043E\u0432"
---

{{< edit_this_page >}}

## Что и почему?

Ассоциативные массивы являются своего рода усиленными массивами, которые позволяют использовать строки в качестве индексов, а не только целые числа. Программисты используют их для работы со сложными структурами данных, что облегчает обработку данных, которые неудобно упорядочивать в последовательный список.

## Как это сделать:

Прежде всего, объявите ассоциативный массив в Bash:

```Bash
declare -A my_array
```

Затем, вы можете начать заполнять его значениями, используя строки в качестве ключей:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Программирование"
```

Чтобы получить доступ к элементу, используйте его ключ:

```Bash
echo ${my_array["name"]}  # Выводит: Linux Journal
```

Итерация по ключам и значениям также проста:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Пример вывода может выглядеть так:

```
name: Linux Journal
topic: Программирование
```

Чтобы добавить или изменить элементы, просто присвойте значение ключу, аналогично начальному заполнению:

```Bash
my_array["readers"]="Вы"
```

И чтобы удалить элемент, используйте `unset`:

```Bash
unset my_array["topic"]
```

## Глубокое погружение

Ассоциативные массивы были введены в Bash версии 4.0 и являются относительно новым дополнением к языку. До их введения работа с массивами, индексы которых не являются целыми числами, была затруднительной и часто требовала обходных путей или использования внешних инструментов, таких как `awk` или `sed`.

Внутри Bash ассоциативные массивы реализованы с использованием хеш-таблиц. Эта реализация обеспечивает эффективный поиск по ключу, который остается довольно постоянным независимо от размера массива, что является критически важной особенностью для производительности при выполнении скриптов.

Хотя ассоциативные массивы в Bash приносят много силы и гибкости в написание скриптов, они имеют свой набор ограничений, таких как несколько громоздкая работа по сравнению с массивами в языках более высокого уровня, таких как Python или JavaScript. Для сложных задач манипуляции с данными может оказаться предпочтительным использование внешних инструментов или языков, которые лучше подходят для задачи.

Тем не менее, для многих типичных задач написания скриптов ассоциативные массивы представляют собой ценный инструмент в арсенале программиста Bash, позволяя создавать более читабельные и удобные в обслуживании скрипты за счет использования значимых строковых ключей вместо числовых индексов.
