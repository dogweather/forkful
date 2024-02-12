---
title:                "Перевірка наявності директорії"
date:                  2024-02-03T19:07:17.034898-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Перевірка наявності директорії в Elixir полягає в підтвердженні наявності директорії за вказаним шляхом в файловій системі. Програмісти роблять це, щоб переконатися, що вони можуть безпечно читати з, записувати в або виконувати операції з директорією, не зустрічаючи помилок через її відсутність.

## Як це зробити:
Стандартна бібліотека Elixir пропонує простий спосіб перевірки наявності директорії через модуль `File`. Ось як ви можете використовувати його:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Директорія існує!"
else
  IO.puts "Директорія не існує."
end
```

Приклад виводу, виходячи з того, що директорії не існує:
```
Директорія не існує.
```

Для більш складних взаємодій з файловою системою, включаючи перевірку наявності директорії, ви можете розглянути можливість використання сторонніх бібліотек, таких як `FileSystem`. Хоча стандартні можливості Elixir достатні для багатьох випадків, `FileSystem` може пропонувати більш тонкий контроль та зворотний зв'язок для складних додатків. Проте, для базової потреби перевірки наявності директорії, рекомендується зазвичай залишатися при використанні нативного модуля `File`, оскільки він доступний без спеціальних залежностей.