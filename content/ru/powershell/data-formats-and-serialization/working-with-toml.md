---
title:                "Работа с TOML"
aliases: - /ru/powershell/working-with-toml.md
date:                  2024-01-29T00:04:56.549406-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

TOML, что является сокращением от Tom's Obvious, Minimal Language (Простой и минималистичный язык Тома), — это формат сериализации данных, который легко читается благодаря своей четкой семантике. Программисты используют его для файлов конфигурации, так как он находит баланс между удобочитаемостью для человека и пригодностью для машины.

## Как:

В PowerShell нет встроенной командлеты для разбора TOML. Обычно используется модуль или преобразование TOML в JSON с помощью инструмента вроде `toml-to-json`, если вы хотите работать с PowerShell. Вот как это делается с помощью вымышленного модуля `PowerShellTOML`:

```PowerShell
# Сначала устанавливаем модуль (вымышленный, для демонстрации)
Install-Module PowerShellTOML

# Импортируем файл TOML
$config = Import-TomlConfig -Path './config.toml'

# Доступ к значению
Write-Output $config.database.server

# Пример содержимого TOML в 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Пример вывода:
# 192.168.1.1
```

## Подробнее

TOML был создан Томом Престоном-Вернером, сооснователем GitHub, как более простая альтернатива XML и YAML для файлов конфигурации. Первая версия появилась в 2013 году. TOML сравним с JSON, но он спроектирован быть более дружелюбным к человеку, что делает его хорошим выбором для конфигурации, поддерживаемой людьми. К альтернативам относятся YAML, JSON и XML.

С точки зрения реализации модуль PowerShell для TOML обычно является оболочкой вокруг библиотеки TOML, написанной на более производительном языке, таком как C#. PowerShell не имеет встроенной поддержки для TOML, поэтому такой модуль необходим для удобного взаимодействия с форматом TOML.

## Смотрите также

- Стандарт TOML: https://toml.io/en/
- Репозиторий на GitHub для модуля `toml` PowerShell (если существует на момент чтения): https://github.com/powershell/PowerShellTOML
- Введение в TOML: https://github.com/toml-lang/toml
- Сравнение форматов сериализации данных: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
