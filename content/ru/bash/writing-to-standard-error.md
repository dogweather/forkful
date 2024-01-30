---
title:                "Запись в стандартный поток ошибок"
date:                  2024-01-29T00:06:02.457785-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Запись в стандартный поток ошибок, `stderr`, позволяет выводить сообщения об ошибках отдельно от стандартного вывода, `stdout`. Программисты используют `stderr` для сообщения об ошибках, не затрагивая при этом обычный вывод команд, что упрощает обработку и логирование ошибок.

## Как сделать:
```
# Перенаправление команды echo в стандартный поток ошибок
echo "Ошибка: Неверный ввод." >&2

# Использование printf для записи в стандартный поток ошибок
printf "Ошибка: Файл не найден.\n" >&2

# Пример скрипта, который записывает как в stdout, так и в stderr
echo "Процесс запущен..."
echo "Ой! Что-то пошло не так." >&2
echo "Процесс завершен."
```
Пример вывода:
```
Процесс запущен...
Процесс завершен.
Ой! Что-то пошло не так.
```
В приведенном примере "Ой! Что-то пошло не так." отправляется в `stderr` и может отображаться не в том порядке, если смешивается со `stdout` в терминале, поскольку `stderr` обычно не буферизуется.

## Подробнее
Bash унаследовал концепцию 'дескрипторов файлов' из Unix, со `stdout` как fd `1` и `stderr` как fd `2`. Перенаправление в `&2` отправляет вывод в `stderr`. Исторически, это разделение позволяло более легко управлять и фильтровать вывод, при этом `2>&1` является общим шаблоном для перенаправления `stderr` в `stdout`. Жизнеспособной альтернативой явной переадресации является использование `logger` для интеграции с syslog или настройка скрипта для внутренней обработки ошибок.

## Смотрите также
- Шпаргалка по перенаправлениям в Bash: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections
- Подробный обзор скриптования в Bash: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/
- Руководство по продвинутому скриптованию в Bash: https://www.tldp.org/LDP/abs/html/