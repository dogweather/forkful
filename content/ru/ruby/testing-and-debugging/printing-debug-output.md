---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:38.413543-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Ruby `puts` \u0438 `p` - \u044D\u0442\u043E \u0432\u0430\u0448\
  \u0438 \u043E\u0441\u043D\u043E\u0432\u043D\u044B\u0435 \u043C\u0435\u0442\u043E\
  \u0434\u044B \u0434\u043B\u044F \u0431\u044B\u0441\u0442\u0440\u043E\u0433\u043E\
  \ \u0432\u044B\u0432\u043E\u0434\u0430 \u0432 \u043A\u043E\u043D\u0441\u043E\u043B\
  \u044C."
lastmod: '2024-03-13T22:44:46.002246-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Ruby `puts` \u0438 `p` - \u044D\u0442\u043E \u0432\u0430\u0448\u0438\
  \ \u043E\u0441\u043D\u043E\u0432\u043D\u044B\u0435 \u043C\u0435\u0442\u043E\u0434\
  \u044B \u0434\u043B\u044F \u0431\u044B\u0441\u0442\u0440\u043E\u0433\u043E \u0432\
  \u044B\u0432\u043E\u0434\u0430 \u0432 \u043A\u043E\u043D\u0441\u043E\u043B\u044C\
  ."
title: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\
  \u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438"
weight: 33
---

## Как это сделать:
В Ruby `puts` и `p` - это ваши основные методы для быстрого вывода в консоль.

```Ruby
def who_said_what
  quote = "Быть или не быть"
  author = "Шекспир"
  puts "Цитата: #{quote}"
  p "Автор: #{author}"
end

who_said_what
```

Пример вывода:

```
Цитата: Быть или не быть
"Автор: Шекспир"
```

Метод `puts` выводит информацию в читаемом формате, добавляя новую строку в конце. В отличие от этого, `p` выводит значение в более сыром виде, что полезно, когда вам нужно увидеть, является ли что-то строкой или нет.

## Подробнее
До появления современных IDE вывод в консоль был методом отладки. Этот старый, но проверенный метод особенно хорош, когда вы хотите избежать лишних затрат времени на настройку отладчика.

В качестве альтернатив, вы можете использовать `pp` для красивой печати сложных объектов или библиотеки гемов, такие как `awesome_print`, для повышения читаемости. Если ваш отладочный вывод становится слишком болтливым, рассмотрите возможность использования библиотеки логирования для контроля уровней болтливости.

С точки зрения реализации, `puts` и `p` пишут в `$stdout`, глобальный поток ввода-вывода в Ruby. Вывод может быть перенаправлен при необходимости. Помните, что, хотя эти методы удобны, чрезмерные отладочные печати могут засорить вашу консоль и усложнить отладку.

## Смотрите также
- Документация Ruby для `Kernel#puts`: https://ruby-doc.org/core/Kernel.html#method-i-puts
- Документация Ruby для `Kernel#p`: https://ruby-doc.org/core/Kernel.html#method-i-p
- Руководство по красивой печати в Ruby: https://ruby-doc.org/stdlib/libdoc/pp/rdoc/PP.html
- Гем Awesome Print для красочного вывода: https://rubygems.org/gems/awesome_print/
