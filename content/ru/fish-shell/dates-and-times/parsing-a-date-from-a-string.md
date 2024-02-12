---
title:                "Анализ даты из строки"
aliases:
- /ru/fish-shell/parsing-a-date-from-a-string.md
date:                  2024-01-29T00:00:23.497141-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ даты из строки"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Разбор даты из строки включает в себя чтение информации о дате, форматированной как текст, и преобразование её в структуру данных даты, которую программа может понять. Программисты делают это для того, чтобы манипулировать датами и работать с ними — думайте об аналитике, планировании или просто о выводе их в другом формате.

## Как это сделать:

```Fish Shell
# Базовый разбор даты с использованием функции `strptime`
set date_string "2023-04-15"
set -l format "%Y-%m-%d"
set -l parsed_date (string tolower (date -u --date=$date_string +"$format"))

echo $parsed_date # Выводит: 2023-04-15
```

```Fish Shell
# Обработка нескольких форматов дат с помощью switch
set date_string1 "15-04-2023"
set date_string2 "Апрель 15, 2023"

function parse_date -a date_string
    switch $date_string
        case "*-*-*"
            date --date=$date_string +%Y-%m-%d
        case "* *, *"
            date --date=$date_string +%Y-%m-%d
    end
end

echo (parse_date $date_string1) # Выводит: 2023-04-15
echo (parse_date $date_string2) # Выводит: 2023-04-15
```

## Глубокое погружение

Fish Shell не имеет встроенных функций разбора дат, как некоторые другие языки. Вместо этого он опирается на внешние утилиты, такие как `date`. Команда `date` универсальна, и с помощью `strptime` (разбор строки времени), которая является стандартной функцией библиотеки C, она может обрабатывать множество форматов дат.

До `date` и `strptime` программисты писали собственные парсеры — часто с ошибками и сложные. Теперь утилиты обрабатывают особенности часовых поясов и високосных годов, избавляя нас от головной боли.

Альтернативы? Конечно, скриптовые языки, такие как Python, имеют мощные библиотеки для работы с датой и временем, например, `datetime`. Но Fish, будучи 'оболочкой', предпочитает легковесные программы для командной строки для такой задачи.

В наших примерах мы использовали `switch`, чтобы выбрать формат даты для разбора командой `date`. Это чисто и расширяемо. Хотите больше форматов? Добавьте больше блоков `case`.

Почему `string tolower` в первом примере? Это связано с обеспечением единообразия, гарантируя, что строка формата и вывод будут однотипно в нижнем регистре. Маленькое прикосновение, но оно иллюстрирует предпочтение Fish к простым строковым операциям.

## Смотрите также

- Страница руководства `date`: `man date`
- Документация Fish Shell по манипуляции со строками: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Общие примеры использования команды date: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
