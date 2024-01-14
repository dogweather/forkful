---
title:                "Fish Shell: Вилучення підрядків"
simple_title:         "Вилучення підрядків"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому
Існує багато ситуацій, коли ви хочете витягнути певну частину тексту з рядка або змінної. Наприклад, ви можете бажати видалити певну підстроку або витягнути корисну інформацію зі стрінгових даних. У цьому випадку, витягнення підстроки є корисною функцією, яка допоможе вам виконати ці завдання з легкістю.

## Як це зробити
```Fish Shell``` має вбудовану функцію для витягнення підстроки ```string sub```, яка має наступний синтаксис:

```
string sub <string> <start-index> <end-index>
```
Тут ```string``` - це змінна або рядок тексту, який ми бажаємо обрізати. ```start-index``` і ```end-index``` - це індекси символів, які ви хочете витягнути. Наприклад, якщо ми маємо рядок "Hello World!", і нам потрібно витягнути слово "World", то наша команда буде виглядати так:

```
set str "Hello World!"
string sub $str 6 10
```

Результат цієї команди буде "World".

## Глибокий занурення
Крім функції ```sub```, у Fish Shell є ще декілька корисних методів для витягнення підстроки. Наприклад, функція ```strim``` дозволяє витягнути підстроку по заданим символам, а ```cut``` дозволяє вибрати певну кількість символів з початку або кінця рядка.

## Дивись також
- [Офіційна документація з витягнення підстроки у Fish Shell](https://fishshell.com/docs/current/cmds/sub.html)
- [Інші корисні функції та приклади витягнення підстрок у Fish Shell](https://www.cyberciti.biz/faq/bash-substring-extract-string-after-characters-on-unix/)
- [Розділ української Fish Shell спільноти на Facebook](https://www.facebook.com/groups/fishshellukraine/)