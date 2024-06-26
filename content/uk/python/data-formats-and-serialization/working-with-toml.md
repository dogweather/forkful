---
date: 2024-01-26 04:26:12.653617-07:00
description: "\u042F\u043A: \u041F\u0435\u0440\u0435\u0434 \u043F\u043E\u0447\u0430\
  \u0442\u043A\u043E\u043C \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C\
  \ \u043F\u0430\u043A\u0435\u0442 `toml` \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\
  \u043E\u0433\u043E\u044E `pip install toml`. \u0414\u0430\u0432\u0430\u0439\u0442\
  \u0435 \u0440\u043E\u0437\u0431\u0435\u0440\u0435\u043C\u043E \u0444\u0430\u0439\
  \u043B TOML."
lastmod: '2024-03-13T22:44:48.621159-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0434 \u043F\u043E\u0447\u0430\u0442\u043A\u043E\
  \u043C \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C \u043F\u0430\
  \u043A\u0435\u0442 `toml` \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\
  \u043E\u044E `pip install toml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

## Як:
Перед початком встановіть пакет `toml` за допомогою `pip install toml`. Давайте розберемо файл TOML:

```python
import toml

# Приклад вмісту TOML у вигляді рядка
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Дати високої якості

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Розбір рядка TOML
parsed_toml = toml.loads(toml_string)

# Доступ до даних
print(parsed_toml['owner']['name'])  # Вивід: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Вивід: [8001, 8001, 8002]
```

## Поглиблений розгляд
TOML було створено Томом Престон-Вернером, одним з засновників GitHub, як більш дружелюбний до користувача формат файлу конфігурації. Він призначений для недвозначного відображення у хеш-таблицю та легкої обробки машинами.

Порівняно з JSON, TOML більш читабельний для конфігураційних файлів і підтримує коментарі. YAML, інша альтернатива, може бути більш компактною, але його залежність від відступів та дрібні проблеми, як от недопустимість використання табуляції, можуть збивати з пантелику.

Що стосується деталей реалізації, значення в TOML є типізованими, що включає рядки, цілі числа, числа з плаваючою комою, булеві значення, дати та час, масиви та таблиці. Все чутливе до регістру. Крім того, TOML підтримує багаторядкові рядки та, починаючи з останньої версії, навіть дозволяє масиви з елементами різних типів.

Python використовує бібліотеку `toml`, яка за принципом API схожа на бібліотеки JSON та YAML. Маємо `toml.load` та `toml.loads` для читання TOML із файлу або рядка відповідно, а також `toml.dump` та `toml.dumps` для запису.

## Дивіться також
- Офіційний GitHub-репозиторій TOML для специфікацій: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Документація бібліотеки `toml` Python: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Реальні приклади використання TOML: файли конфігурації менеджера пакетів Rust `cargo` або інструменту упаковки Python `poetry`.
