---
aliases:
- /ru/powershell/sending-an-http-request/
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:33.273520-07:00
description: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u2014 \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\
  \u0431 \u0437\u0430\u043F\u0440\u043E\u0441\u0438\u0442\u044C \u0434\u0430\u043D\
  \u043D\u044B\u0435 \u0438\u043B\u0438 \u043F\u043E\u043B\u0443\u0447\u0438\u0442\
  \u044C \u043E\u0442\u0432\u0435\u0442 \u043E\u0442 \u0432\u0435\u0431-\u0441\u0435\
  \u0440\u0432\u0438\u0441\u0430. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E\
  \ \u0434\u043B\u044F \u0432\u0437\u0430\u0438\u043C\u043E\u0434\u0435\u0439\u0441\
  \u0442\u0432\u0438\u044F \u0441 API, \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\
  \u044F\u2026"
lastmod: 2024-02-18 23:08:57.254153
model: gpt-4-0125-preview
summary: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u2014 \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\
  \u0431 \u0437\u0430\u043F\u0440\u043E\u0441\u0438\u0442\u044C \u0434\u0430\u043D\
  \u043D\u044B\u0435 \u0438\u043B\u0438 \u043F\u043E\u043B\u0443\u0447\u0438\u0442\
  \u044C \u043E\u0442\u0432\u0435\u0442 \u043E\u0442 \u0432\u0435\u0431-\u0441\u0435\
  \u0440\u0432\u0438\u0441\u0430. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E\
  \ \u0434\u043B\u044F \u0432\u0437\u0430\u0438\u043C\u043E\u0434\u0435\u0439\u0441\
  \u0442\u0432\u0438\u044F \u0441 API, \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\
  \u044F\u2026"
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса — это способ запросить данные или получить ответ от веб-сервиса. Программисты делают это для взаимодействия с API, получения содержимого веб-сайта или связи с удаленными серверами.

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
