---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:15.103997-07:00
description: "\u042F\u043A: \u0423 Google Apps Script \u043F\u0435\u0440\u0432\u0438\
  \u043D\u043D\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\u0431 \u043D\u0430\u0434\
  \u0456\u0441\u043B\u0430\u0442\u0438 HTTP-\u0437\u0430\u043F\u0438\u0442 - \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0441\u0435\u0440\
  \u0432\u0456\u0441\u0443 `UrlFetchApp`. \u0426\u0435\u0439 \u0441\u0435\u0440\u0432\
  \u0456\u0441 \u043D\u0430\u0434\u0430\u0454 \u043C\u0435\u0442\u043E\u0434\u0438\
  \ \u0434\u043B\u044F \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F HTTP\
  \ GET \u0456 POST\u2026"
lastmod: '2024-03-13T22:44:48.505023-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Google Apps Script \u043F\u0435\u0440\u0432\u0438\u043D\u043D\u0438\
  \u0439 \u0441\u043F\u043E\u0441\u0456\u0431 \u043D\u0430\u0434\u0456\u0441\u043B\
  \u0430\u0442\u0438 HTTP-\u0437\u0430\u043F\u0438\u0442 - \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0441\u0435\u0440\u0432\u0456\u0441\
  \u0443 `UrlFetchApp`."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## Як:
У Google Apps Script первинний спосіб надіслати HTTP-запит - використання сервісу `UrlFetchApp`. Цей сервіс надає методи для створення HTTP GET і POST запитів. Ось простий приклад створення GET запиту для отримання даних JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Для POST запиту, який загальноприйнято використовується для надсилання даних на сервер, потрібно включити більше деталей у параметр опцій:

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
    // Конвертування JavaScript об'єкта в рядок JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Ці фрагменти показують базове реалізацію GET і POST запитів. Вивід залежатиме від відповіді API і може бути переглянутий у Logger Google Apps Script.

## Занурення в глибину
Сервіс `UrlFetchApp` в Google Apps Script значно еволюціонував з моменту свого створення, пропонуючи більш тонкий контроль над HTTP-запитами з такими функціями як встановлення заголовків, навантаження, та обробка multipart/form-data для завантаження файлів. Хоча він забезпечує прямий спосіб інтеграції зовнішніх веб-сервісів, розробники, що прийшли з більш потужних серверних мов, можуть вважати його функціональність дещо обмеженою порівняно з такими бібліотеками як `requests` в Python або API `fetch` в JavaScript для Node.js.

Однією з помітних обмежень є ліміт часу виконання для Google Apps Script, що впливає на довготривалі запити. Крім того, хоча `UrlFetchApp` покриває широкий спектр сценаріїв використання, більш складні ситуації, що включають OAuth автентифікацію або обробку дуже великих навантажень, можуть потребувати креативних рішень або використання додаткових ресурсів Google Cloud.

Проте, для більшості інтеграцій, з якими зустрічаються розробники Google Workspace - від автоматизації отримання даних до публікації оновлень в зовнішні служби - `UrlFetchApp` надає потужний, доступний інструмент. Його інтеграція в Google Apps Script означає, що немає необхідності в зовнішніх бібліотеках або складних налаштуваннях, роблячи HTTP-запити відносно простими для виконання в межах можливостей Google Apps Script. З розширенням ландшафту веб-API `UrlFetchApp` залишається критичним мостом для програм Google Apps Script для взаємодії зі світом за межами екосистеми Google.
