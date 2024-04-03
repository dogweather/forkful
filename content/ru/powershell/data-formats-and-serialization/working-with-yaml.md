---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:29.379924-07:00
description: "\u041A\u0430\u043A? \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\
  \u043E\u0442\u0430\u0442\u044C \u0441 YAML \u0432 PowerShell, \u0432\u0430\u043C\
  \ \u043D\u0443\u0436\u043D\u043E \u0431\u0443\u0434\u0435\u0442 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u043C\u043E\u0434\u0443\u043B\
  \u044C, \u0442\u0430\u043A\u043E\u0439 \u043A\u0430\u043A `powershell-yaml`. \u0421\
  \u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\u0442\u0430\u043D\u043E\u0432\u0438\
  \u0442\u0435 \u0435\u0433\u043E."
lastmod: '2024-03-13T22:44:45.489875-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 YAML \u0432 PowerShell, \u0432\u0430\u043C \u043D\u0443\u0436\u043D\
  \u043E \u0431\u0443\u0434\u0435\u0442 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u0442\u044C \u043C\u043E\u0434\u0443\u043B\u044C, \u0442\u0430\
  \u043A\u043E\u0439 \u043A\u0430\u043A `powershell-yaml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как?
Чтобы работать с YAML в PowerShell, вам нужно будет использовать модуль, такой как `powershell-yaml`. Сначала установите его:

```PowerShell
Install-Module -Name powershell-yaml
```

Чтение содержимого YAML:

```PowerShell
# Импортируйте модуль
Import-Module powershell-yaml

# Загрузите файл YAML
$yamlContent = Get-Content -Path 'config.yaml' -Raw

# Преобразуйте YAML в объект PowerShell
$configObject = ConvertFrom-Yaml -Yaml $yamlContent

# Выведите объект
$configObject
```

Создание и запись YAML:

```PowerShell
# Создайте хеш-таблицу
$person = @{
  name = 'Jane Doe'
  age = 30
  languages = @('English', 'French')
}

# Преобразуйте хеш-таблицу в YAML
$yamlOutput = ConvertTo-Yaml -Data $person

# Запишите YAML в файл
$yamlOutput | Out-File -FilePath 'person.yaml'
```

## Подробнее
YAML появился в начале 2000-х и означает "YAML Ain't Markup Language" (YAML — это не язык разметки), что является рекурсивным акронимом, подчеркивающим его ориентацию на данные в отличие от языков разметки, таких как HTML. Хотя JSON часто является предпочтительным выбором для API и веб-сервисов из-за его эффективности разбора и компактности, YAML остается популярным из-за его читабельности и большей доступности для редактирования человеком, особенно в файлах конфигурации (например, Docker Compose и Kubernetes).

Альтернативы `powershell-yaml` включают `YamlDotNet` с кодом на `.NET`, или ручной разбор строк YAML - но зачем усложнять себе жизнь?

Внутри `powershell-yaml` использует `YamlDotNet`, преобразуя YAML в объекты .NET, с которыми PowerShell может легко работать. Это взаимодействие позволяет плавно переводить данные YAML в экосистему PowerShell.

## Смотрите также
- [`powershell-yaml` на PowerShell Gallery](https://www.powershellgallery.com/packages/powershell-yaml)
- [Официальный сайт YAML](https://yaml.org/)
- [Справочник по синтаксису YAML](https://learnxinyminutes.com/docs/yaml/)
