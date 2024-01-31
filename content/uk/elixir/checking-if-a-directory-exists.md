---
title:                "Перевірка наявності директорії"
date:                  2024-01-19
html_title:           "Bash: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Перевірка чи існує директорія – це процес визначення, чи є певний шлях у файловій системі директорією. Програмісти роблять це, щоб уникнути помилок при спробі доступу або модифікації файлів у неіснуючій директорії.

## How to: (Як робити:)
```elixir
# Підключаємо модуль File
File.dir?("path/to/directory")

# Приклад 1: перевірка існуючої директорії
File.dir?("/etc") 
# => true

# Приклад 2: перевірка неіснуючої директорії
File.dir?("/some/nonexistent/directory") 
# => false
```

## Deep Dive (Поглиблено)
Історично, перевірка існування директорії у більшості мов програмування є частиною стандартної бібліотеки. В Elixir це забезпечує модуль `File` зі своїми функціями. Крім `File.dir?`, можна використовувати `File.stat/2` для отримання більш детальної інформації. Щодо реалізації, Elixir використовує виклики операційної системи, щоб визначити інформацію про файли та директорії. Це особливо корисно в сценаріях, коли потрібна перевірка перед створенням, видаленням або переміщенням директорій.

## See Also (Дивіться також)
- [Elixir File Module Documentation](https://hexdocs.pm/elixir/File.html)
- [File.stat/2 Documentation](https://hexdocs.pm/elixir/File.html#stat/2)
