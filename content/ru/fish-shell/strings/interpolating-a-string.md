---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:00.086292-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Fish \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\
  \u0442\u0435 \u0434\u0432\u043E\u0439\u043D\u044B\u0435 \u043A\u0430\u0432\u044B\
  \u0447\u043A\u0438 \u0438 \u043F\u043E\u043C\u0435\u0441\u0442\u0438\u0442\u0435\
  \ \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u0443\u044E \u0438\u043B\u0438\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u0443, \u043A\u043E\u0442\u043E\u0440\u0443\
  \u044E \u0445\u043E\u0442\u0438\u0442\u0435 \u0438\u043D\u0442\u0435\u0440\u043F\
  \u043E\u043B\u0438\u0440\u043E\u0432\u0430\u0442\u044C, \u0441 \u043F\u043E\u043C\
  \u043E\u0449\u044C\u044E \u0437\u043D\u0430\u043A\u0430 \u0434\u043E\u043B\u043B\
  \u0430\u0440\u0430 `$`\u2026"
lastmod: '2024-03-13T22:44:45.810611-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Fish \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\
  \u0435 \u0434\u0432\u043E\u0439\u043D\u044B\u0435 \u043A\u0430\u0432\u044B\u0447\
  \u043A\u0438 \u0438 \u043F\u043E\u043C\u0435\u0441\u0442\u0438\u0442\u0435 \u043F\
  \u0435\u0440\u0435\u043C\u0435\u043D\u043D\u0443\u044E \u0438\u043B\u0438 \u043A\
  \u043E\u043C\u0430\u043D\u0434\u0443, \u043A\u043E\u0442\u043E\u0440\u0443\u044E\
  \ \u0445\u043E\u0442\u0438\u0442\u0435 \u0438\u043D\u0442\u0435\u0440\u043F\u043E\
  \u043B\u0438\u0440\u043E\u0432\u0430\u0442\u044C, \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E \u0437\u043D\u0430\u043A\u0430 \u0434\u043E\u043B\u043B\u0430\
  \u0440\u0430 `$` \u043D\u0435\u043F\u043E\u0441\u0440\u0435\u0434\u0441\u0442\u0432\
  \u0435\u043D\u043D\u043E \u0432 \u0441\u0442\u0440\u043E\u043A\u0435."
title: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 8
---

## Как это сделать:
В Fish используйте двойные кавычки и поместите переменную или команду, которую хотите интерполировать, с помощью знака доллара `$` непосредственно в строке.

```fish
set name "world"
echo "Привет, $name!"
```

Вывод:
```
Привет, мир!
```

Чтобы включить вывод команды внутри строки:

```fish
echo "У меня есть (count (ls)) файлов в этой директории."
```

Вывод может быть:
```
У меня есть 9 файлов в этой директории.
```

Переменные и команды оцениваются и аккуратно вставляются на место, куда вы их поставили.

## Глубокое погружение
До появления Fish и других современных оболочек часто приходилось использовать более громоздкую комбинацию кавычек и конкатенации или полагаться на внешние инструменты для внедрения переменных в строки.

В bash, например, это выглядело бы так:

```bash
name="world"
echo "Привет, "$name"!"
```

Не так уж и изящно, правда?

Fish не только упрощает этот процесс, но и более изящно обрабатывает ошибки. Если переменная не существует, Fish вставит пустую строку, уменьшая вероятность сбоя из-за неправильной интерполяции.

Альтернативы прямой интерполяции включают использование команды `printf`:

```fish
set animal "нарвал"
printf "Нарвал - замечательное существо!" $animal
```

Вывод:
```
Нарвал - замечательное существо!
```

В этом случае `%s` является заполнителем для строковой переменной `$animal`, который заменяется командой `printf`.

С точки зрения реализации, когда Fish обрабатывает командную строку, он анализирует строки в двойных кавычках и заменяет переменные их значениями на лету. Это элегантно и напоминает интерполяцию переменных, найденную в языках высокого уровня, таких как Ruby или PHP.

## Смотрите также
Для получения дополнительной информации о манипуляциях со строками и скриптинге в Fish ознакомьтесь с этим:

- [Документация по оболочке Fish: Кавычки](https://fishshell.com/docs/current/index.html#quotes)
- [Учебник по оболочке Fish](https://fishshell.com/docs/current/tutorial.html)
- [Stack Overflow: Как использовать переменные в команде в Fish](https://stackoverflow.com/questions/2763006/how-to-use-variables-in-a-command-in-fish)
