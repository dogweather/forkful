---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:21.279295-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\u043F\u0438\
  \u0441 \u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456 YAML \u0443 Python \u0437\
  \u0430\u0437\u0432\u0438\u0447\u0430\u0439 \u0432\u0438\u043C\u0430\u0433\u0430\u0454\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0441\
  \u0442\u043E\u0440\u043E\u043D\u043D\u044C\u043E\u0457 \u0431\u0456\u0431\u043B\u0456\
  \u043E\u0442\u0435\u043A\u0438, \u0441\u0435\u0440\u0435\u0434 \u044F\u043A\u0438\
  \u0445 `PyYAML` \u0454 \u043D\u0430\u0439\u043F\u043E\u043F\u0443\u043B\u044F\u0440\
  \u043D\u0456\u0448\u043E\u044E. \u0429\u043E\u0431\u2026"
lastmod: '2024-03-13T22:44:48.616570-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\u043F\
  \u0438\u0441 \u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456 YAML \u0443 Python\
  \ \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439 \u0432\u0438\u043C\u0430\u0433\
  \u0430\u0454 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  \ \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044C\u043E\u0457 \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0441\u0435\u0440\u0435\u0434 \u044F\
  \u043A\u0438\u0445 `PyYAML` \u0454 \u043D\u0430\u0439\u043F\u043E\u043F\u0443\u043B\
  \u044F\u0440\u043D\u0456\u0448\u043E\u044E."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

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
