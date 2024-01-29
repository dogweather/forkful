---
title:                "Запись в стандартный поток ошибок"
date:                  2024-01-29T00:06:29.282082-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Стандартный поток ошибок (`stderr`) - это поток, отдельный от стандартного выхода (`stdout`), который используется в основном для вывода сообщений об ошибках или диагностики. Программисты используют его, чтобы предотвратить смешивание сообщений об ошибках с обычным выводом программы, что помогает как в отладке, так и в обработке вывода.

## Как это сделать:
В Ruby вы можете писать в стандартный поток ошибок, используя `$stderr.puts` или его сокращённую форму `STDERR.puts`. Вот простой пример:

```ruby
puts "Это пойдёт в стандартный вывод."
$stderr.puts "Это пойдёт в стандартный поток ошибок."

# Сокращённая версия:
STDERR.puts "Это тоже пойдёт в стандартный поток ошибок."
```

Откройте терминал, запустите скрипт и заметьте, что по умолчанию всё всё равно отображается вместе. Чтобы разделить потоки, требуется перенаправление. Вот как это можно сделать:

```shell
ruby your_script.rb >output.txt 2>error.txt
```

Эта команда перенаправляет стандартный вывод в `output.txt` и стандартный поток ошибок в `error.txt`.

## Подробнее
Концепция `stderr` возвращает нас к самым ранним дням Unix. Она предназначена для сообщений об ошибках, чтобы они оставались видимыми, даже когда `stdout` перенаправлен. Хотя `$stderr.puts` и `STDERR.puts` являются распространёнными в Ruby, существуют и другие способы записи в `stderr`, такие как использование `warn` для вывода предупреждений или более низкоуровневых API вроде `$stderr.write`. С точки зрения реализации, `stderr` по умолчанию не буферизуется, что обеспечивает немедленный вывод, в то время как `stdout` обычно буферизуется.

## Смотрите также
- Документация Ruby по I/O: [https://ruby-doc.org/core-3.1.2/IO.html](https://ruby-doc.org/core-3.1.2/IO.html)
- Базовые спецификации Open Group (стандартные потоки UNIX): [https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html)
- Понимание перенаправления в Shell скриптах: [https://www.gnu.org/software/bash/manual/html_node/Redirections.html](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
