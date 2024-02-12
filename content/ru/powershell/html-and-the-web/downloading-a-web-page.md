---
title:                "Загрузка веб-страницы"
aliases:
- ru/powershell/downloading-a-web-page.md
date:                  2024-01-28T23:57:33.624649-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Скачивание веб-страницы означает изъятие ее содержимого через сеть. Программисты делают это для веб-скрапинга, просмотра в режиме офлайн или автоматизации взаимодействий с веб-сайтами.

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
