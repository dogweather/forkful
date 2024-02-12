---
title:                "Робота з YAML"
aliases:
- /uk/python/working-with-yaml/
date:                  2024-02-03T19:27:21.279295-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
YAML, що означає "YAML Ain't Markup Language" (YAML - це не мова розмітки), є форматом серіалізації даних, зручним для читання людиною. Програмісти використовують YAML для файлів конфігурації, міжпроцесного обміну повідомленнями та зберігання даних через його простий синтаксис та легкість читання порівняно з іншими форматами, такими як XML або JSON.

## Як це зробити:
Читання та запис у форматі YAML у Python зазвичай вимагає використання сторонньої бібліотеки, серед яких `PyYAML` є найпопулярнішою. Щоб розпочати, потрібно встановити PyYAML, виконавши `pip install PyYAML`.

**Приклад: Запис у файл YAML**

```python
import yaml

data = {'список': [1, 42, 3.141, 1337, 'допомога', u'€'],
        'рядок': 'бух!',
        'ще один словник': {'фу': 'бар', 'ключ': 'значення', 'відповідь': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Це створює `example.yaml` з даними, структурованими у форматі YAML.
```

**Приклад: Читання з файлу YAML**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# Вивід: 
# {'список': [1, 42, 3.141, 1337, 'допомога', '€'],
#  'рядок': 'бух!',
#  'ще один словник': {'фу': 'бар', 'ключ': 'значення', 'відповідь': 42}}
```

**Використання YAML для конфігурації**

Багато програмістів використовують YAML для управління конфігураціями програм. Ось приклад, як можна структурувати конфігураційний файл та прочитати його:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

Читання конфігураційного файлу в Python:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # Вивід: localhost
```

**Обробка складних структур**

Для обробки складних структур PyYAML дозволяє визначати користувацькі об'єкти Python. Проте, з метою забезпечення безпеки, використовуйте `safe_load`, щоб уникнути виконання довільних функцій або об'єктів.

```python
import yaml

# Визначення об'єкта Python
class Example:
    def __init__(self, value):
        self.value = value

# Користувацький конструктор
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# Додавання конструктора для тега "!example"
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'дані'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # Вивід: дані
```

У цьому фрагменті `!example` є користувацьким тегом, який використовується для інстанціювання об'єкта `Example` зі значенням 'дані' з рядка YAML. Такі користувацькі завантажувачі, як цей, підвищують гнучкість PyYAML, дозволяючи обробляти більш складні структури даних та типи.
