---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:09.040689-07:00
description: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u043A\u043E\u0434\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u0435 \u0438\u043C\u0435\u043D\u0438 \u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044F \u0438 \u043F\u0430\
  \u0440\u043E\u043B\u044F \u0432 \u0437\u0430\u0433\u043E\u043B\u043E\u0432\u043E\
  \u043A \u0437\u0430\u043F\u0440\u043E\u0441\u0430 \u0434\u043B\u044F \u0434\u043E\
  \u0441\u0442\u0443\u043F\u0430 \u043A \u0437\u0430\u0449\u0438\u0449\u0435\u043D\
  \u043D\u044B\u043C \u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C.\u2026"
lastmod: '2024-03-13T22:44:44.190304-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u043A\u043E\u0434\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u0435 \u0438\u043C\u0435\u043D\u0438 \u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044F \u0438 \u043F\u0430\
  \u0440\u043E\u043B\u044F \u0432 \u0437\u0430\u0433\u043E\u043B\u043E\u0432\u043E\
  \u043A \u0437\u0430\u043F\u0440\u043E\u0441\u0430 \u0434\u043B\u044F \u0434\u043E\
  \u0441\u0442\u0443\u043F\u0430 \u043A \u0437\u0430\u0449\u0438\u0449\u0435\u043D\
  \u043D\u044B\u043C \u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C.\u2026"
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса с базовой аутентификацией включает кодирование имени пользователя и пароля в заголовок запроса для доступа к защищенным ресурсам. Программисты используют этот метод для аутентификации на стороне сервера, чтобы интегрироваться с API, которые требуют базовой аутентификации для операций, таких как получение данных или размещение контента.

## Как это сделать:

В Google Apps Script для отправки HTTP-запроса с базовой аутентификацией используется сервис `UrlFetchApp` в сочетании с заголовком авторизации в кодировке base64. Вот пошаговое руководство:

1. **Кодирование Учетных Данных**: Сначала закодируйте ваше имя пользователя и пароль в формате base64. Google Apps Script не имеет собственной функции кодирования строк в формате base64, поэтому для этой цели используется Utilities.base64Encode.

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Настройка Параметров Запроса**: С закодированными учетными данными подготовьте объект параметров для HTTP-запроса, включая метод и заголовки.

```javascript
var options = {
  method: 'get', // или 'post', 'put', в зависимости от ваших потребностей
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // дополнительные параметры, такие как 'muteHttpExceptions' для обработки ошибок, могут быть добавлены здесь
};
```

3. **Выполнение Запроса**: Используйте метод `UrlFetchApp.fetch` с целевым URL и объектом параметров.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

Пример вывода при успешном запросе будет варьироваться в зависимости от ответа API. Для API, работающего с JSON, вы можете увидеть что-то вроде:

```
{"status":"Success","data":"Resource data here..."}
```

Убедитесь, что вы обработали возможные HTTP-ошибки, проверив код ответа или используя опцию `muteHttpExceptions` для более контролируемого управления ошибками.

## Глубокое Погружение

Отправка HTTP-запроса с базовой аутентификацией была стандартным методом во многих языках программирования для доступа к веб-ресурсам, требующим аутентификации. В контексте Google Apps Script `UrlFetchApp` предоставляет простой способ выполнения этих HTTP-запросов, включая те, которые требуют аутентификации. Включение базовых учетных данных в заголовки запроса является простым, но эффективным методом, однако сопряжено с проблемами безопасности, в первую очередь потому, что учетные данные отправляются в незашифрованном виде, только в кодировке base64, которую можно легко декодировать при перехвате.

Для повышения безопасности рекомендуются альтернативы, такие как OAuth 2.0, особенно при работе с конфиденциальными данными или операциями. В Google Apps Script встроена поддержка OAuth 2.0 с использованием библиотеки `OAuth2`, что упрощает процесс аутентификации против сервисов, поддерживающих этот протокол.

Несмотря на свои ограничения в области безопасности, базовая аутентификация широко используется для простых или внутренних приложений, которые не подвергаются воздействию большого интернета. Ее простота реализации, поскольку она требует только одного запроса с правильно установленными заголовками, делает ее привлекательным вариантом для быстрой интеграции или для API, где методы повышенной безопасности недоступны. Однако, программистам рекомендуется учитывать проблемы безопасности и искать более безопасные альтернативы, когда они доступны.
