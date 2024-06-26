---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:50.640963-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041F\u043E\u0437\u043D\u0430\u043A\u043E\u043C\u044C\u0442\u0435\
  \u0441\u044C \u043F\u043E\u0431\u043B\u0438\u0436\u0435 \u0441 `echo` \u2013 \u0448\
  \u0432\u0435\u0439\u0446\u0430\u0440\u0441\u043A\u0438\u043C \u043D\u043E\u0436\u043E\
  \u043C \u0434\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430 \u0432 Fish. \u0412\
  \u043E\u0442 \u043A\u0430\u043A \u0434\u043E\u0431\u0430\u0432\u0438\u0442\u044C\
  \ \u043D\u0435\u043C\u043D\u043E\u0433\u043E \u043E\u0442\u043B\u0430\u0434\u043E\
  \u0447\u043D\u044B\u0445 \u043F\u0435\u0447\u0430\u0442\u0435\u0439 \u0432 \u0432\
  \u0430\u0448\u0438 shell-\u0441\u043A\u0440\u0438\u043F\u0442\u044B."
lastmod: '2024-03-13T22:44:45.844955-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u0437\u043D\u0430\u043A\u043E\u043C\u044C\u0442\u0435\u0441\
  \u044C \u043F\u043E\u0431\u043B\u0438\u0436\u0435 \u0441 `echo` \u2013 \u0448\u0432\
  \u0435\u0439\u0446\u0430\u0440\u0441\u043A\u0438\u043C \u043D\u043E\u0436\u043E\u043C\
  \ \u0434\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430 \u0432 Fish."
title: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\
  \u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438"
weight: 33
---

## Как это сделать:
Познакомьтесь поближе с `echo` – швейцарским ножом для вывода в Fish. Вот как добавить немного отладочных печатей в ваши shell-скрипты.

```Fish Shell
function greet
    set name $argv[1]
    echo "Привет, $name! Давай отлаживать."
    echo "Запущена функция greet" >&2
end

greet "Ада"
```
Пример вывода:
```
Привет, Ада! Давай отлаживать.
Запущена функция greet
```
Стандартный вывод (`stdout`) – это главная сцена вашего скрипта, но для отладочного болтовни используйте стандартную ошибку (`stderr`) с использованием `>&2`.

## Подробнее
Когда мониторы были такими же глубокими, как и широкими, вывод был на вес золота. Стандартный вывод (`stdout`) стал чистым, ориентированным на пользователя каналом, в то время как стандартная ошибка (`stderr`) превратилась в тёмный переулок для разговоров программистов, например, для вывода отладочной информации.

В Fish стандартные команды вывода – это `echo`, `printf` и `print`. `Echo` используется за его простоту и в основном для простых сообщений и встроенной отладки.

Но вам не обязательно ограничиваться только `echo`. Предпочитайте `printf` для форматированных строк или используйте перенаправление (`>` или `>>`), чтобы выгружать отладочную информацию в файл для последующего просмотра.

Что касается реализации, использование `stderr` для вывода отладочной информации – это конвенция из мира Unix, помогающая отделить зёрна (фактический вывод) от плевел (отладочный шум). Это означает, что пользователи могут все ещё перенаправлять реальный вывод вашего скрипта, не получая вмешательства отладочного мусора.

## Смотрите также
- Документация Fish Shell о [Командах](https://fishshell.com/docs/current/commands.html)
- StackOverflow: Обсуждения и примеры [отладки в Fish](https://stackoverflow.com/questions/tagged/fish)
- Wiki Грега: Подробная информация о [перенаправлении ввода/вывода](https://mywiki.wooledge.org/BashGuide/InputAndOutput#Redirection)
