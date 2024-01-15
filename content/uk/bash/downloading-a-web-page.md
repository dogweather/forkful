---
title:                "Завантаження веб-сторінки"
html_title:           "Bash: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Причина:
##Why
 Завантаження веб-сторінки є необхідним для отримання потрібної інформації або виконання певної задачі.
## How To
```Bash
#!/bin/bash
# Завантаження веб-сторінки
wget <URL>
# Виведення вмісту на екран
cat <page_name>.html
```
Використовуючи команду "wget", ми можемо завантажити веб-сторінку за допомогою командного рядка Bash. Потім, використовуючи команду "cat", ми можемо вивести вміст сторінки на екран.

## Deep Dive
Загалом, команда "wget" слугує для завантаження файлів з інтернету. Вона підтримує багато опцій, таких як використання проксі, обмеження швидкості завантаження та інше. Для докладнішої інформації, ви можете ознайомитися з документацією "wget".

## See Also
[Офіційна документація "wget"](https://www.gnu.org/software/wget/)
[Стаття про використання команди "wget"](https://www.howtogeek.com/432981/how-to-use-wget-the-ultimate-command-line-downloading-tool/)