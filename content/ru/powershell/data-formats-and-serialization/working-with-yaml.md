---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:29.379924-07:00
description: "YAML \u2014 \u044D\u0442\u043E \u0443\u0434\u043E\u0431\u043D\u044B\u0439\
  \ \u0434\u043B\u044F \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u0430 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\
  \u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u044E\u0442 \u0435\u0433\u043E \u0434\u043B\u044F \u0444\u0430\u0439\u043B\
  \u043E\u0432 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\u0438\
  , \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  \ \u043C\u0435\u0436\u0434\u0443 \u044F\u0437\u044B\u043A\u0430\u043C\u0438, \u0430\
  \ \u0442\u0430\u043A\u0436\u0435\u2026"
lastmod: '2024-03-13T22:44:45.489875-06:00'
model: gpt-4-0125-preview
summary: "YAML \u2014 \u044D\u0442\u043E \u0443\u0434\u043E\u0431\u043D\u044B\u0439\
  \ \u0434\u043B\u044F \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u0430 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\
  \u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u044E\u0442 \u0435\u0433\u043E \u0434\u043B\u044F \u0444\u0430\u0439\u043B\
  \u043E\u0432 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\u0438\
  , \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  \ \u043C\u0435\u0436\u0434\u0443 \u044F\u0437\u044B\u043A\u0430\u043C\u0438, \u0430\
  \ \u0442\u0430\u043A\u0436\u0435\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
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
