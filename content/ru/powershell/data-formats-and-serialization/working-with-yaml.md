---
title:                "Работа с YAML"
aliases:
- /ru/powershell/working-with-yaml.md
date:                  2024-01-29T00:05:29.379924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
YAML — это удобный для человека формат сериализации данных. Программисты используют его для файлов конфигурации, обмена данными между языками, а также потому, что он легче для чтения и записи по сравнению с XML или JSON.

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
