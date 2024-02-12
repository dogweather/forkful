---
title:                "Робота з TOML"
aliases:
- /uk/powershell/working-with-toml.md
date:                  2024-01-26T04:25:44.198830-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і чому?

TOML, що є скороченням від Tom's Obvious, Minimal Language (Очевидна, Мінімальна Мова Тома), являє собою формат серіалізації даних, який легко читати завдяки його чіткій семантиці. Програмісти використовують його для файлів конфігурації, оскільки він досягає балансу між зручністю для людини і пристосованістю для машини.

## Як робити:

У PowerShell немає рідної командлети для розбору TOML. Зазвичай, ви б використовували модуль або конвертували TOML в JSON за допомогою інструменту, такого як `toml-to-json`, якщо ви хочете працювати з PowerShell. Ось як ви б могли це зробити з уявним модулем `PowerShellTOML`:

```PowerShell
# Спочатку встановіть модуль (уявний, для демонстрації)
Install-Module PowerShellTOML

# Імпортування файлу TOML
$config = Import-TomlConfig -Path './config.toml'

# Доступ до значення
Write-Output $config.database.server

# Приклад вмісту TOML у 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Приклад виводу:
# 192.168.1.1
```

## Поглиблений огляд

TOML був створений Томом Престон-Вернером, співзасновником GitHub, як спрощена альтернатива XML і YAML для файлів конфігурації. Його перша версія з'явилася в 2013 році. TOML можна порівняти з JSON, але він розроблений бути більш дружнім до людей, що робить його хорошим вибором для конфігурації, яку підтримують люди. Альтернативи включають YAML, JSON і XML.

З точки зору впровадження, модуль PowerShell для TOML, як правило, буде обгорткою навколо бібліотеки TOML, написаної на більш орієнтованій на продуктивність мові, такій як C#. PowerShell не має вбудованої підтримки для TOML, саме тому такий модуль є необхідним, щоб зручно взаємодіяти з форматом TOML.

## Дивіться також

- Стандарт TOML: https://toml.io/en/
- GitHub репозиторій для `toml` модуля PowerShell (якщо існує на час читання): https://github.com/powershell/PowerShellTOML
- Вступ до TOML: https://github.com/toml-lang/toml
- Порівняння форматів серіалізації даних: https://uk.wikipedia.org/wiki/%D0%9F%D0%BE%D1%80%D1%96%D0%B2%D0%BD%D1%8F%D0%BD%D0%BD%D1%8F_%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%82%D1%96%D0%B2_%D1%81%D0%B5%D1%80%D1%96%D0%B0%D0%BB%D1%96%D0%B7%D0%B0%D1%86%D1%96%D1%97_%D0%B4%D0%B0%D0%BD%D0%B8%D1%85
