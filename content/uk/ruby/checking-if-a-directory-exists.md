---
title:                "Перевірка наявністі директорії"
html_title:           "Ruby: Перевірка наявністі директорії"
simple_title:         "Перевірка наявністі директорії"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому 

 Перевірка існування директорії є важливою для програмістів, оскільки це дозволяє перевірити наявність необхідного шляху та зробити відповідну обробку помилок в разі його відсутності.

## Як

Для перевірки існування директорії у Ruby існує спеціальний метод `Dir.exist?()`. Приклад його використання виглядає наступним чином:

```Ruby
if Dir.exist?("path/to/directory")
  puts "Directory exists!"
else
  puts "Directory does not exist."
end
```
В даному прикладі, ми перевіряємо наявність директорії за шляхом "path/to/directory". Якщо директорія існує, то програма виведе "Directory exists!" в консоль, інакше - "Directory does not exist."

## Глибоке погруження

Цей метод ґрунтується на наявності директорії в файловій системі. Якщо шлях, переданий методу, веде до директорії - метод повертає `true`, інакше - `false`. Також варто зазначити, що цей метод можна використовувати не тільки для локальних директорій, але і для доступу до директорій у мережі, за допомогою правильного шляху.

## Дивитися також

- [Ruby документація про `Dir.exist?()`](https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F)
- [Блог стаття з прикладами використання методу `Dir.exist?()`](https://www.rubyguides.com/2018/10/check-if-directory-exists/)