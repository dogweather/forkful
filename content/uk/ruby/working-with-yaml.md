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

## Чому

ЯML (YAML) є мовою розмітки даних, яка дозволяє легко створювати та редагувати структуровані файли. Вона особливо корисна для збереження конфігурацій та налаштувань програм.

## Як це зробити

Для початку, потрібно встановити гем `yaml` за допомогою команди: `gem install yaml`. Після цього, можна приступити до створення та редагування яМЛ файли за допомогою наступного коду:

```Ruby
require 'yaml'

# Створення нового ЯМЛ файлу
yaml_file = YAML.dump({name: "John", age: 25})

# Читання з ЯМЛ файлу та виведення на екран
new_file = YAML.load(yaml_file)
puts new_file[:name] #=> "John"
puts new_file[:age] #=> 25
```

## Profoundly Speakeasy

ЯМЛ файли можуть бути корисними не тільки для збереження простих даних, але й для структурованих об'єктів, таких як хеші та масиви. Крім того, ЯМЛ дозволяє вкладати об'єкти один в одного, що робить його більш потужним і гнучким. Нижче наведений приклад вкладеного об'єкта в ЯМЛ файлі:

```Ruby
require 'yaml'

# Вкладений хеш
nested_hash = {
  name: "Samantha",
  age: 30,
  education: {
    degree: "Masters",
    major: "Computer Science"
  }
}

# Записуємо вкладений об'єкт до ЯМЛ файлу
yaml_file = YAML.dump(nested_hash)

# Зчитуємо з ЯМЛ файлу та виводимо на екран
new_file = YAML.load(yaml_file)
puts new_file[:name] #=> "Samantha"
puts new_file[:education][:degree] #=> "Masters"
```

## Детальний аналіз

ЯМЛ (YAML) має простий та читабельний синтаксис, який дозволяє легко створювати структуровані файли. Він також підтримує переваги інших форматів даних, таких як JSON, і може бути використаний для обміну даними між різними мовами програмування. Крім того, ЯМЛ дозволяє використовувати коментарі, щоб пояснити структуру файлу та розуміти його зміст.

## Дивись також

- [YAML - вікіпедія](https://uk.wikipedia.org/wiki/YAML)
- [Офіційний сайт мови YAML](https://yaml.org/)
- [Документація по роботі з YAML в Рубі](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html)