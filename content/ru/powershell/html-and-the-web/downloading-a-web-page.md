---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:33.624649-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043C\u0430\u0433\u0438\u0447\u0435\u0441\u043A\
  \u043E\u0435 \u0437\u0430\u043A\u043B\u0438\u043D\u0430\u043D\u0438\u0435 \u0434\
  \u043B\u044F \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u0441 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C PowerShell.\
  \ \u041C\u044B \u0432\u043E\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\
  \u0441\u044F `Invoke-WebRequest`."
lastmod: '2024-03-13T22:44:45.449128-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043C\u0430\u0433\u0438\u0447\u0435\u0441\u043A\u043E\
  \u0435 \u0437\u0430\u043A\u043B\u0438\u043D\u0430\u043D\u0438\u0435 \u0434\u043B\
  \u044F \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u044F \u0432\u0435\
  \u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u0441 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C PowerShell."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Вот магическое заклинание для извлечения веб-страницы с использованием PowerShell. Мы воспользуемся `Invoke-WebRequest`.

```PowerShell
# Забираем содержимое example.com
$response = Invoke-WebRequest -Uri "http://example.com"

# Вот что вы получили
$response.Content
```

Пример вывода:

```PowerShell
<!doctype html>
<html>
<head>
    <title>Пример Домена</title>
    ...
    <!-- и так далее -->
</head>
...
</html>
```

Возможно, вас интересует только текст, без HTML тегов. Давайте сделаем это:

```PowerShell
# Только текст, пожалуйста
$response.ParsedHtml.body.innerText
```

## Глубокое Погружение
Когда-то PowerShell не имел крутой командлет `Invoke-WebRequest`. Программисты использовали класс .NET `System.Net.WebClient` или прибегали к внешним инструментам. Теперь это все встроено, что упрощает нам задачи.

`Invoke-WebRequest` предлагает больше, чем просто содержимое. Заголовки, статус и информация о сессии - это все там. Если вы работаете с API, вам понравится `Invoke-RestMethod` как специализированная альтернатива.

Внутри, эти командлеты полагаются на мощный класс .NET HttpClient, обеспечивающий надежность и обширный функционал.

И, если вы нетерпеливы в ожидании скачивания веб-страницы, `Invoke-WebRequest` также поддерживает асинхронные операции. Однако, это тема для другого разговора.

## Смотрите Также
- [Документация по Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- Больше о [Invoke-RestMethod для взаимодействия с API](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Репозиторий PowerShell на GitHub](https://github.com/PowerShell/PowerShell) для любопытных программистов, которым нравится заглядывать под капот.
