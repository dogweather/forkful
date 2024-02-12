---
title:                "Разбор HTML"
aliases:
- /ru/powershell/parsing-html.md
date:                  2024-01-29T00:00:10.384742-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Парсинг HTML означает анализ содержимого HTML для извлечения конкретных данных. Программисты делают это для автоматизации веб-скрапинга, добычи данных или для интеграции веб-контента в приложения.

## Как это сделать:
Давайте извлечем некоторые данные со страницы в интернете. Мы будем использовать Invoke-WebRequest, а затем отфильтруем то, что нам нужно.

```PowerShell
# Получаем содержимое страницы
$response = Invoke-WebRequest -Uri "http://example.com"

# Парсим содержимое HTML
$parsedHtml = $response.ParsedHtml

# Извлекаем данные
# Допустим, мы хотим получить все тексты гиперссылок
$links = $parsedHtml.getElementsByTagName('a') | ForEach-Object { $_.innerText }
$links
```

Пример вывода:

```
Главная
О нас
Услуги
Контакт
```

## Подробнее
Исторически парсинг HTML в PowerShell мог быть неуклюжим. У вас был выбор использования regex (печально известного для HTML), COM объектов с Internet Explorer или сторонних библиотек. Теперь, cmdlet Invoke-WebRequest в PowerShell упрощает процесс, интегрируясь с движком Internet Explorer для парсинга HTML — хотя это немного медленно и громоздко.

Существуют альтернативы, такие как библиотека HtmlAgilityPack, которая гораздо более надежна и точно настроена для парсинга HTML. Она требует дополнительной настройки, но окупается гибкостью и производительностью.

С точки зрения реализации, стоит отметить, что подход PowerShell не всегда точен для динамического содержимого, заполняемого JavaScript. Для работы с динамическим содержимым вам может потребоваться инструмент автоматизации браузера, такой как Selenium.

## Смотрите также
- [HtmlAgilityPack на GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Selenium с PowerShell](https://github.com/adamdriscoll/selenium-powershell)
