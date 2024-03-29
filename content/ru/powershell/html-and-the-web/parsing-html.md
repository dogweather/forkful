---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:10.384742-07:00
description: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0441\u043E\u0434\
  \u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E HTML \u0434\u043B\u044F \u0438\u0437\
  \u0432\u043B\u0435\u0447\u0435\u043D\u0438\u044F \u043A\u043E\u043D\u043A\u0440\u0435\
  \u0442\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\
  \u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u043C\u0430\
  \u0442\u0438\u0437\u0430\u0446\u0438\u0438 \u0432\u0435\u0431-\u0441\u043A\u0440\
  \u0430\u043F\u0438\u043D\u0433\u0430, \u0434\u043E\u0431\u044B\u0447\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0438\u043B\u0438\u2026"
lastmod: '2024-03-13T22:44:45.447319-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0441\u043E\u0434\
  \u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E HTML \u0434\u043B\u044F \u0438\u0437\
  \u0432\u043B\u0435\u0447\u0435\u043D\u0438\u044F \u043A\u043E\u043D\u043A\u0440\u0435\
  \u0442\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\
  \u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u043C\u0430\
  \u0442\u0438\u0437\u0430\u0446\u0438\u0438 \u0432\u0435\u0431-\u0441\u043A\u0440\
  \u0430\u043F\u0438\u043D\u0433\u0430, \u0434\u043E\u0431\u044B\u0447\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0438\u043B\u0438\u2026"
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
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
