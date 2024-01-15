---
title:                "Перевірка наявності каталогу."
html_title:           "Elixir: Перевірка наявності каталогу."
simple_title:         "Перевірка наявності каталогу."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Тому, що

Перевірка існування директорії допомагає уникнути помилок і забезпечити правильність дій в програмах, які потребують доступу до файлів та директорій.

## Як

```Elixir
File.exists?("directory_name") 
# повертає true або false в залежності від того, чи існує директорія
```

```Elixir
if File.exists?("directory_name") do
  # виконувати код якщо директорія існує
else
  # виконувати код якщо директорія не існує
end
```

## Deep Dive

При перевірці існування директорії, Elixir використовує функцію `File.exists?` з модуля `File`. Ця функція перевіряє, чи існує файл чи директорія за допомогою системного виклику `stat`.

Будьте уважні, використовуючи функцію `File.exists?`, оскільки вона може повернути неправильний результат, коли існуючий файл або директорія видалена після перевірки.

## See Also

- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir File.exists? function documentation](https://hexdocs.pm/elixir/File.html#exists?/1)
- [Elixir `File.stat/1` system call documentation](https://erlang.org/doc/man/file.html#stat-1)