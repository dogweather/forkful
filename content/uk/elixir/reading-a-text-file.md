---
title:                "Читання текстового файлу."
html_title:           "Elixir: Читання текстового файлу."
simple_title:         "Читання текстового файлу."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому 

Текстові файли є невід'ємною частиною багатьох проектів та програм і їх читання може допомогти вирішити багатьох завдань. Ця стаття описує як читати текстовий файл з використанням мови програмування Elixir.

## Як 

Основною функцією для читання текстових файлів є `File.read/1` яка приймає назву файлу в якості аргументу і повертає вміст файлу. Наприклад: 

```Elixir
  file_content = File.read("myfile.txt")
```

Якщо файл не існує, то функція повертає атом `:error`. Щоб перевірити чи існує файл, можна використовувати функцію `File.exists?/1`. 

```Elixir 
  if File.exists?("myfile.txt") do 
    file_content = File.read("myfile.txt")
  else 
    IO.puts "Файл не існує!"
  end 
```

Також можна задавати шлях до файлу відносно поточної робочої директорії за допомогою функції `File.cwd/0` та оператора `/`, наприклад: 

```Elixir 
  file_content = File.read("#{File.cwd}/textfiles/myfile.txt")
```

Вміст файлу читається як бінарний рядок, але його можна перетворити у список рядків за допомогою функції `String.split/3`: 

```Elixir
  file_content = File.read("myfile.txt")
  lines = String.split(file_content, "\n", trim: true)
```

Значення `trim: true` дозволяє видалити пробіли на початку та в кінці кожного рядка. 

## Поглиблення 

Якщо потрібно читати більш складний формат файлу, наприклад CSV, можна скористатися пакетом [CSV](https://hexdocs.pm/csv). Для цього потрібно додати пакет до вашого проекту та скомпілювати: 

```Elixir 
  def deps do 
    [{:csv, "~> 2.2"}] 
  end 
```

Приклад читання CSV файлу: 

```Elixir 
  file_content = File.read("mycsvfile.csv")
  csv = CSV.decode(file_content)
  # Результат буде містити список списків, де кожен список представляє один рядок з файлу
```

## Дивитись Також 

- [Функція File.read/1](https://hexdocs.pm/elixir/File.html#read/1)
- [Спеціальний модуль для роботи з текстовими файлами - IO](https://hexdocs.pm/elixir/IO.html)
- [Модуль CSV для роботи з CSV файли](https://hexdocs.pm/csv)