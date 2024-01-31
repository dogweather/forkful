---
title:                "Робота з TOML"
date:                  2024-01-26T04:19:22.710053-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і Чому?
TOML, скорочення від Tom's Obvious, Minimal Language, це формат серіалізації даних. Програмісти полюбляють його за простоту і читабельність; ідеально підходить для конфігураційних файлів, подібно до YAML, але менш громіздко, ніж JSON для людини.

## Як це зробити:
Перш за все, встановіть `toml-cli`, щоб працювати з TOML у Bash. Зручно для читання або редагування TOML файлів на льоту.

```Bash
# Встановіть toml-cli, наш маленький помічник для завдань з TOML
pip install toml-cli

# Уявіть, що у вас є файл TOML, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Прочитайте значення
toml get config.toml owner.name
# Вивід: Tom

# Встановіть значення
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Професійна порада: Використовуйте лапки для ключів з крапками або спеціальними символами!
```

## Поглиблений Розгляд
Народившись з нелюбові до незручностей JSON для людей, TOML з'явився приблизно в 2013 році. Том Престон-Вернер, співзасновник GitHub, хотів чогось надзвичайно зрозумілого. Як альтернативи були YAML та INI, але TOML це ніби найкраще з обох. 

Вам відкриті вкладені дані та масиви, мінус "пастки на ноги" YAML і фігурні дужки JSON. TOML зараз є основою для конфігурації в Cargo Rust, що свідчить про його популярність у світі розробників. Він керується специфікацією, що тримає все чітко та добре визначено. Ви знайдете парсери майже на будь-якій мові, що робить його широко прийнятим. 

## Дивіться також
- Офіційний репозиторій TOML на GitHub: https://github.com/toml-lang/toml
- toml-cli на PyPI: https://pypi.org/project/toml-cli/
- Порівняння форматів серіалізації даних: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
