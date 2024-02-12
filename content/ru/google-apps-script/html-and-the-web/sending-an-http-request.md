---
title:                "Отправка HTTP-запроса"
date:                  2024-02-01T22:01:56.414139-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/google-apps-script/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса в Google Apps Script заключается в программном создании вызова к внешнему веб-серверу или API. Программисты делают это для получения или отправки данных веб-сервисам, интегрируя огромный спектр веб-ресурсов и функциональных возможностей непосредственно в свои проекты Google Apps Script.

## Как:

В Google Apps Script основной способ отправки HTTP-запроса - использование сервиса `UrlFetchApp`. Этот сервис предоставляет методы для выполнения HTTP-запросов методом GET и POST. Вот простой пример выполнения GET-запроса для получения данных в формате JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Для выполнения POST-запроса, который обычно используется для отправки данных на сервер, необходимо включить дополнительные данные в параметр options:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Преобразование объекта JavaScript в строку JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Эти фрагменты показывают базовую реализацию выполнения GET и POST-запросов. Результат будет зависеть от ответа API и может быть просмотрен в Logger Google Apps Script.

## Погружение

Сервис `UrlFetchApp` в Google Apps Script значительно эволюционировал с момента своего создания, предлагая более тонкий контроль над HTTP-запросами с функциями, такими как задание заголовков, тела запроса и обработка multipart/form-data для загрузки файлов. Хотя он обеспечивает простой способ интеграции внешних веб-сервисов, разработчики, перешедшие на него с более мощных серверных языков, могут считать его функциональность в некоторой степени ограниченной по сравнению с библиотеками вроде `requests` в Python или `fetch` API в Node.js.

Одно из заметных ограничений - это лимит времени выполнения для Google Apps Script, который влияет на длительные запросы. Кроме того, несмотря на то, что `UrlFetchApp` охватывает широкий спектр сценариев использования, более сложные ситуации, включающие аутентификацию OAuth или обработку очень больших массивов данных, могут требовать нестандартных решений или использования дополнительных ресурсов Google Cloud.

Тем не менее, для большинства интеграций, с которыми сталкиваются разработчики Google Workspace, начиная от автоматизации получения данных и заканчивая публикацией обновлений во внешних сервисах, `UrlFetchApp` представляет собой мощный, доступный инструмент. Его интеграция в Google Apps Script означает, что нет необходимости во внешних библиотеках или сложной настройке, что делает выполнение HTTP-запросов относительно простым в рамках ограничений Google Apps Script. Поскольку ландшафт веб-API продолжает расширяться, `UrlFetchApp` остается критическим мостом для программ Google Apps Script для взаимодействия с миром за пределами экосистемы Google.
