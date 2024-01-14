---
title:                "Fish Shell: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Програмістів часом змушують приводити рядки тексту до "формату заголовків". Наприклад, коли потрібно вивести назву користувача чи назву категорії. Використання функції capitalizing дозволяє зробити це швидко та просто.

## Як

```Fish Shell
# Використання капіталізації для першої літери кожного слова
set first_name "oleksandr" 
capitalizing $first_name # Вивід: Oleksandr 

# Використання капіталізації для всього слова
set country "ukraine" 
capitalizing -w $country # Вивід: Ukraine 
```

## Глибше

Функція capitalizing використовує вбудовану утиліту "case" для зміни регістру літер. Натомість, опція -w використовує команду "string" для капіталізації всього слова. Крім того, існує можливість використовувати опцію -s для капіталізації лише першої літери, інші літери залишаються в нижньому регістрі.

## Дивись також

- [Документація Fish Shell про capitalizing](https://fishshell.com/docs/current/cmds/capitalizing.html)
- [Стаття про перетворення регістру літер у Fish Shell](https://blog.fishshell.com/transforming-case/)
- [Пріоритети операторів в Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_operator_precedence)