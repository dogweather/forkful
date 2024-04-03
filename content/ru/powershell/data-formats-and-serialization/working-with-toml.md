---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:56.549406-07:00
description: "\u041A\u0430\u043A: \u0412 PowerShell \u043D\u0435\u0442 \u0432\u0441\
  \u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u043A\u043E\u043C\u0430\u043D\u0434\
  \u043B\u0435\u0442\u044B \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\
  \u0430 TOML. \u041E\u0431\u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u0442\u0441\u044F \u043C\u043E\u0434\u0443\u043B\u044C\
  \ \u0438\u043B\u0438 \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\
  \u0430\u043D\u0438\u0435 TOML \u0432 JSON \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\
  \u044E \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0430 \u0432\
  \u0440\u043E\u0434\u0435 `toml-\u2026"
lastmod: '2024-03-13T22:44:45.494791-06:00'
model: gpt-4-0125-preview
summary: "\u0412 PowerShell \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0439 \u043A\u043E\u043C\u0430\u043D\u0434\u043B\u0435\u0442\u044B\
  \ \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 TOML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

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
