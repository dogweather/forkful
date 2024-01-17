---
title:                "Робота з yaml"
html_title:           "Ruby: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

Що & Чому?
Робота з YAML - це набір інструментів для роботи з текстовими даними у структурованому форматі. Це корисно для програмістів, оскільки дозволяє легко зберігати та обмінюватися даними між різними програмами та мовами програмування.

Як зробити:
Щоб почати роботу з YAML, вам знадобиться встановити бібліотеку `yaml` за допомогою `gem install yaml` команди. Після цього, ви можете використовувати методи `parse` та `load` для зчитування YAML файлів та приведення їх до рубішних об'єктів. Нижче наведено приклади коду та вихідних даних:

```ruby
require 'yaml'

# зчитування YAML файлу
data = YAML.load(File.read('data.yaml'))

# виведення даних
puts data

# збереження об'єкту у YAML форматі
new_data = { name: 'John', age: 25 }
puts new_data.to_yaml
```

Нижче можна побачити приклади вихідних даних у двох випадках:

```ruby
# дані у вигляді рубішних об'єктів
data = { name: 'John', age: 25 }
puts data

# дані у форматі YAML
---
:age: 25
:name: John
```

Глибша Інформація:
YAML був розроблений як простий та зручний формат для обміну даними, який був відсутній у форматах XML та JSON. Його написано на мові програмування Python та базується на ідей простоти та читабельності. Існують також інші бібліотеки, які дозволяють працювати з YAML файлами у різних мовах програмування, таких як JavaScript та Java.

Подивися також:
- Офіційна документація: https://ruby-doc.org/stdlib-1.9.3/libdoc/yaml/rdoc/YAML.html
- Огляд YAML: https://yaml.com/