---
title:                "Завантаження веб-сторінки"
html_title:           "Ruby: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки є необхідною задачею для багатьох розробників, оскільки це дозволяє отримувати дані з Інтернету та використовувати їх у своїх програмах.

## Як виконати

Для завантаження веб-сторінки у Ruby, використовуйте функцію `open` з модуля `open-uri`. Нижче наведені приклади коду, які демонструють це у дії:

```ruby
require 'open-uri'

# Завантаження веб-сторінки та виведення її вмісту
page = open('https://www.example.com').read
puts page
```

```ruby
require 'open-uri'

# Збереження веб-сторінки як файл
open('https://www.example.com') do |f|
  File.open('example.html', 'w') do |file|
    file.write(f.read)
  end
end
```
Вивід:

```html
<!doctype html>
<html>
<head>
  <title>Example Domain</title>
  ...
</body>
</html>
```

## Глибші деталі

Функція `open` з модуля `open-uri` дозволяє отримати доступ до веб-сторінки як до звичайного потоку даних і використовувати його для отримання різноманітної інформації, такої як заголовки, статус відповіді та додаткові деталі. Крім того, ви можете вказати додаткові параметри, такі як метод запиту та тип MIME, для збільшення контролю над процесом завантаження.

## Дивіться також

- [Документація Ruby модуля OpenURI](https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html)
- [Вступ до програмування на Ruby](https://www.codeschool.com/learn/ruby)
- [Розділ Ruby на Stack Overflow](https://stackoverflow.com/questions/tagged/ruby)