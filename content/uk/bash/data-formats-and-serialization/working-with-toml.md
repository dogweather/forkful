---
date: 2024-01-26 04:19:22.710053-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u0435\u0440\u0448 \u0437\u0430 \u0432\u0441\u0435, \u0432\u0441\u0442\u0430\
  \u043D\u043E\u0432\u0456\u0442\u044C `toml-cli`, \u0449\u043E\u0431 \u043F\u0440\
  \u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437 TOML \u0443 Bash. \u0417\u0440\
  \u0443\u0447\u043D\u043E \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\
  \u044F \u0430\u0431\u043E \u0440\u0435\u0434\u0430\u0433\u0443\u0432\u0430\u043D\
  \u043D\u044F TOML \u0444\u0430\u0439\u043B\u0456\u0432 \u043D\u0430 \u043B\u044C\
  \u043E\u0442\u0443."
lastmod: '2024-03-13T22:44:49.617590-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0448 \u0437\u0430 \u0432\u0441\u0435, \u0432\u0441\u0442\
  \u0430\u043D\u043E\u0432\u0456\u0442\u044C `toml-cli`, \u0449\u043E\u0431 \u043F\
  \u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437 TOML \u0443 Bash."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

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
