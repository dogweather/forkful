---
title:                "Перевірка наявності директорії"
html_title:           "Fish Shell: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що та Навіщо?

Перевірка наявності директорії - це процес пошуку певної папки в системі за її шляхом. Програмісти роблять це, щоб розуміти, чи можуть вони доступатись до файлів у цій папці, чи потрібно створити нову.

## Як це зробити:

Виконаємо перевірку за допомогою стандартної бібліотеки `lfs` (LuaFileSystem). Якщо вона встановлена в вашому проекті, код буде виглядати так:

```Lua
lfs = require('lfs')

function directory_exists(path)
  -- Перевірка, чи існує директорія
  if lfs.attributes(path, "mode") == "directory" then
    return true
  else
    return false
  end
end

print(directory_exists("/path/to/directory"))  -- Виведе 'true' або 'false'
```

## Поглиблений розділ

Lua вперше впровадила цей метод перевірки директорії в 2003 році з введенням бібліотеки `LuaFileSystem`. Оскільки Lua - це мова без вбудованої підтримки взаємодії з файловою системою, `lfs` часто використовується для цієї мети. 

Альтернативно, можна використовувати виклики операційних систем для перевірки існування директорії, але цей метод може працювати по-різному в різних ОС.

Насправді `lfs.attributes` викликає вбудовану функцію C `stat`, яка отримує різну інформацію про файл або директорію. Пареметр `mode` в `lfs.attributes(path, "mode")` повертає тип файлу.

## Див. також

[Документація LuaFileSystem](https://keplerproject.github.io/luafilesystem/)

[Lua 5.3 Короткий курс](https://www.lua.org/pil/1.html)

[Stack Overflow: Як перевірити, чи існує директорія в Lua?](https://stackoverflow.com/questions/1340230/check-if-directory-exists-in-lua)