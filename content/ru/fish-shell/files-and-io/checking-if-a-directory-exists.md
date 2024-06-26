---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:21.795311-07:00
description: "\u041A\u0430\u043A \u0432\u044B\u043F\u043E\u043B\u043D\u0438\u0442\u044C\
  : \u041F\u0440\u043E\u0432\u0435\u0440\u044C\u0442\u0435 \u0441\u0443\u0449\u0435\
  \u0441\u0442\u0432\u043E\u0432\u0430\u043D\u0438\u0435 \u0434\u0438\u0440\u0435\u043A\
  \u0442\u043E\u0440\u0438\u0438 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043A\u043E\u043C\u0430\u043D\u0434\
  \u044B `test`."
lastmod: '2024-03-13T22:44:45.866262-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u043E\u0432\u0435\u0440\u044C\u0442\u0435 \u0441\u0443\u0449\
  \u0435\u0441\u0442\u0432\u043E\u0432\u0430\u043D\u0438\u0435 \u0434\u0438\u0440\u0435\
  \u043A\u0442\u043E\u0440\u0438\u0438 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\
  \u044E \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043A\u043E\u043C\u0430\u043D\
  \u0434\u044B `test`."
title: "\u041F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u0441\u0443\u0449\u0435\u0441\
  \u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\
  \u043E\u0440\u0438\u0438"
weight: 20
---

## Как выполнить:
Проверьте существование директории с помощью простой команды `test`:
```Fish Shell
if test -d /path/to/dir
    echo "Директория существует"
else
    echo "Такой директории нет"
end
```
Пример вывода, когда директория существует:
```
Директория существует
```
Пример вывода, когда директории не существует:
```
Такой директории нет
```

## Подробнее
Команда `test` (`[ ]` в POSIX-оболочках) является частью систем, подобных Unix, уже десятилетиями. В Fish, `test -d` проверяет наличие директории. Этот метод лучше, чем опирание на вывод команд вроде `ls`, которые могут быть несогласованными или многословными.

Альтернативы:
- `status` может определить, удачно ли выполнилась предыдущая команда, например, `cd /path/to/dir`. Однако, это не рекомендуется использовать чисто для проверки существования, так как это меняет состояние оболочки.
- Внешние инструменты вроде `find` или языки скриптов (Python, Ruby) могут выполнить похожие задачи, но часто они избыточны для простых проверок.

Детали реализации:
Встроенная команда `test` в Fish эффективна и надежна. Она избегает распространенных подводных камней при вызове внешних команд и предоставляет простой синтаксис.

## Смотрите также
- Документация Fish Shell по `test`: https://fishshell.com/docs/current/cmds/test.html
- Спецификация POSIX для `test`: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html
- Обсуждение проверки существования файла: https://unix.stackexchange.com/questions/590694/checking-if-a-directory-exists-in-unix-shell-scripting
