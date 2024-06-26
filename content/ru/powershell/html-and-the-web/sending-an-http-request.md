---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:33.273520-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0435 \u0440\
  \u0443\u043A\u043E\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u043F\u043E \u043E\
  \u0442\u043F\u0440\u0430\u0432\u043A\u0435 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\
  \u043E GET-\u0437\u0430\u043F\u0440\u043E\u0441\u0430."
lastmod: '2024-03-13T22:44:45.445589-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0435 \u0440\u0443\
  \u043A\u043E\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u043F\u043E \u043E\u0442\
  \u043F\u0440\u0430\u0432\u043A\u0435 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E\
  \ GET-\u0437\u0430\u043F\u0440\u043E\u0441\u0430."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
Вот простое руководство по отправке простого GET-запроса:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response
```

И если вы хотите отправить некоторые данные методом POST:

```PowerShell
$body = @{
    'name' = 'Джейн Доу'
    'occupation' = 'Космический рейнджер'
}

$response = Invoke-RestMethod -Uri 'https://api.example.com/users' -Method Post -Body ($body | ConvertTo-Json)
Write-Output $response
```

Пример вывода:

```
name         occupation
----         ----------
Джейн Доу    Космический рейнджер
```

## Погружение в детали:
Отправка HTTP-запроса возвращает нас к заре веб-разработки. Вы вступаете в диалог с вебом на его родном языке, HTTP. Cmdlet `Invoke-RestMethod` в PowerShell является инструментом выбора здесь. До появления `Invoke-RestMethod`, `Invoke-WebRequest` был популярен, и он все еще существует для получения более детальных ответов.

Если вам захочется экспериментировать, у вас есть альтернативы, такие как `curl` или класс `HttpClient` в .NET. Помните, что при использовании `Invoke-RestMethod`, он является оберткой вокруг классов и методов `HttpClient` в .NET, предлагая простоту, но жертвуя некоторым контролем на низком уровне.

С точки зрения реализации, помните, что HTTP-запросы поддерживают различные методы, такие как `GET`, `POST`, `PUT` и т. д. Настраивайте заголовки с помощью `-Headers`, и управляйте тайм-аутами и аутентификацией с помощью дополнительных параметров по мере необходимости. Всегда проверяйте входные данные, если вы используете контент, созданный пользователями, чтобы избежать атак инъекциями.

## См. также:
- [О Invoke-RestMethod в PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Детали `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Понимание REST API](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [Класс `.NET HttpClient`](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
