---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:29.210581-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0438\u043C\u0438\u0442\u0438\u0440\
  \u043E\u0432\u0430\u0442\u044C \u043F\u0440\u043E\u0446\u0435\u0441\u0441 \u0447\
  \u0442\u0435\u043D\u0438\u044F \u0430\u0440\u0433\u0443\u043C\u0435\u043D\u0442\u043E\
  \u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\u0442\u0440\
  \u043E\u043A\u0438 \u0432 Google Apps Script, \u043E\u0441\u043E\u0431\u0435\u043D\
  \u043D\u043E \u0434\u043B\u044F \u0432\u0435\u0431-\u043F\u0440\u0438\u043B\u043E\
  \u0436\u0435\u043D\u0438\u0439, \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.220064-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0438\u043C\u0438\u0442\u0438\u0440\u043E\
  \u0432\u0430\u0442\u044C \u043F\u0440\u043E\u0446\u0435\u0441\u0441 \u0447\u0442\
  \u0435\u043D\u0438\u044F \u0430\u0440\u0433\u0443\u043C\u0435\u043D\u0442\u043E\u0432\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\u0442\u0440\u043E\
  \u043A\u0438 \u0432 Google Apps Script, \u043E\u0441\u043E\u0431\u0435\u043D\u043D\
  \u043E \u0434\u043B\u044F \u0432\u0435\u0431-\u043F\u0440\u0438\u043B\u043E\u0436\
  \u0435\u043D\u0438\u0439, \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u043F\u0430\u0440\
  \u0430\u043C\u0435\u0442\u0440\u044B \u0441\u0442\u0440\u043E\u043A\u0438 \u0437\
  \u0430\u043F\u0440\u043E\u0441\u0430."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 23
---

## Как это сделать:
Чтобы имитировать процесс чтения аргументов командной строки в Google Apps Script, особенно для веб-приложений, вы можете использовать параметры строки запроса. Когда пользователь доступает к URL веб-приложения, вы можете добавить аргументы, такие как `?name=John&age=30`, и анализировать их внутри вашего кода Apps Script. Вот как вы можете это настроить:

```javascript
function doGet(e) {
  var params = e.parameter; // Получает параметры строки запроса
  var name = params['name']; // Получает параметр 'name'
  var age = params['age']; // Получает параметр 'age'

  // Пример вывода:
  var output = "Имя: " + name + ", Возраст: " + age;
  return HtmlService.createHtmlOutput(output);
}

// Пример URL: https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

Когда вы обращаетесь к URL с указанными параметрами, скрипт выдает что-то вроде:

```
Имя: John, Возраст: 30
```

Такой подход является инструментальным для создания персонализированных взаимодействий в веб-приложениях или программного управления выполнением скриптов.

## Погружение
Аргументы командной строки, как их понимают в контексте традиционных языков программирования, предоставляют возможности для скриптов и приложений обработать параметры времени выполнения, тем самым обеспечивая гибкое и динамичное выполнение кода на основе ввода пользователя или автоматизированных процессов. Google Apps Script, являясь облачным языком сценариев для разработки легковесных приложений в экосистеме Google Workspace, не работает нативно через интерфейс командной строки. Вместо этого его выполнение в основном основано на событиях или запускается вручную через UI Apps Script и Google Workspace, или через веб-приложения, которые могут анализировать параметры URL как псевдо аргументы командной строки.

Учитывая это архитектурное различие, программистам, пришедшим из языков с активным использованием CLI, возможно, придется корректировать свой подход при автоматизации задач или разработке приложений в Google Apps Script. Вместо традиционного разбора аргументов командной строки, использование функциональности веб-приложений Google Apps Script или даже пользовательских функций Google Sheets для интерактивной обработки данных может служить аналогичным целям. Хотя это может показаться ограничением сначала, это способствует разработке более дружественных интерфейсов и доступных веб-приложений, соответствуя фокусу Google Apps Script на бесшовной интеграции и расширении приложений Google Workspace.

В сценариях, где точное имитирование поведения CLI имеет первостепенное значение (например, при автоматизации задач с динамическими параметрами), разработчики могут исследовать использование внешних платформ, которые вызывают веб-приложения Google Apps Script, передавая параметры через URL как метод "командной строки". Однако для нативных проектов Google Apps Script обычно приводит к более простым и поддерживаемым решениям принятие модели, ориентированной на события и UI платформы.
