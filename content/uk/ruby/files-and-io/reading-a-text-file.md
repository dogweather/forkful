---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:55:19.884737-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Читання текстового файлу — це процес отримання даних з файла, що містить текст. Програмісти роблять це, щоб обробляти, аналізувати дані або завантажувати конфігурації.

## How to: (Як це робити:)
```Ruby
# Відкриймо текстовий файл і прочитаємо його вміст у змінну
File.open("sample.txt") do |file|
  content = file.read
  puts content
end

# Вивід:
# Вміст вашого файла тут...

# Прочитаємо файл пострічно
File.foreach("sample.txt") do |line|
  puts line
end

# Вивід:
# Перша строка вашого файла...
# Друга строка вашого файла...
# ...і так далі, поки файл не закінчиться
```

## Deep Dive (Поглиблений Розгляд)
Читання файлів - давня потреба в програмуванні. Раніше це вимагало більше коду та розуміння низькорівневих операцій. Сьогодні Ruby має зручні методи для роботи з файлами. 

Є варіанти: використання `File.read` для завантаження всього вмісту файла в пам'ять або `File.foreach` для читання пострічно, що економить пам'ять. Коли файл занадто великий, краще читати пострічно.

Ruby внутрішньо використовує буферизацію для ефективного читання файлів, а також пропонує різні режими (наприклад, "r" для читання, "w" для запису), які допомагають контролювати, як ви взаємодієте з файлами.

## See Also (Додатково)
- [Ruby class IO documentation](https://ruby-doc.org/core-3.1.2/IO.html)
- [Ruby class File documentation](https://ruby-doc.org/core-3.1.2/File.html)
- [Ruby-Doc.org для загальної документації мови Ruby](https://www.ruby-doc.org)