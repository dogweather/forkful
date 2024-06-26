---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:13.162883-07:00
description: "\u041A\u0430\u043A: Google Apps Script, \u0431\u0443\u0434\u0443\u0447\
  \u0438 \u043E\u0441\u043D\u043E\u0432\u0430\u043D\u043D\u044B\u043C \u043D\u0430\
  \ JavaScript, \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\u0442 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0442\u0440\u0430\u0434\
  \u0438\u0446\u0438\u043E\u043D\u043D\u044B\u0439 \u043E\u043F\u0435\u0440\u0430\u0442\
  \u043E\u0440 `try-catch` \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\
  \u0442\u043A\u0438 \u043E\u0448\u0438\u0431\u043E\u043A, \u0430 \u0442\u0430\u043A\
  \u0436\u0435\u2026"
lastmod: '2024-03-13T22:44:44.205104-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, \u0431\u0443\u0434\u0443\u0447\u0438 \u043E\u0441\u043D\
  \u043E\u0432\u0430\u043D\u043D\u044B\u043C \u043D\u0430 JavaScript, \u043F\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0435\u0442 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u0442\u044C \u0442\u0440\u0430\u0434\u0438\u0446\u0438\u043E\u043D\
  \u043D\u044B\u0439 \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440 `try-catch`\
  \ \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u043E\
  \u0448\u0438\u0431\u043E\u043A, \u0430 \u0442\u0430\u043A\u0436\u0435 `finally`,\
  \ \u0435\u0441\u043B\u0438 \u0442\u0440\u0435\u0431\u0443\u0435\u0442\u0441\u044F\
  \ \u043E\u0447\u0438\u0441\u0442\u043A\u0430 \u043D\u0435\u0437\u0430\u0432\u0438\
  \u0441\u0438\u043C\u043E \u043E\u0442 \u0443\u0441\u043F\u0435\u0445\u0430 \u0438\
  \u043B\u0438 \u043E\u0448\u0438\u0431\u043A\u0438."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как:
Google Apps Script, будучи основанным на JavaScript, позволяет использовать традиционный оператор `try-catch` для обработки ошибок, а также `finally`, если требуется очистка независимо от успеха или ошибки.

```javascript
function myFunction() {
  try {
    // Код, который может вызвать ошибку
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("Ячейка A1 пуста.");
    }
    Logger.log(data);
  } catch (e) {
    // Код обработки ошибок
    Logger.log("Ошибка: " + e.message);
  } finally {
    // Код очистки, выполняемый независимо от того, произошла ошибка или нет
    Logger.log("Функция завершена.");
  }
}
```

Пример вывода без ошибки:
```
[Значение ячейки]
Функция завершена.
```

Пример вывода с ошибкой (предполагая, что A1 пуст):
```
Ошибка: Ячейка A1 пуста.
Функция завершена.
```

Google Apps Script также поддерживает создание пользовательских ошибок с использованием объекта `Error` и перехват конкретных типов ошибок при необходимости. Однако отсутствие продвинутой категоризации ошибок делает необходимым опираться на сообщения об ошибках для специфичности.

## Глубокое погружение
Исторически, обработка ошибок в языках сценариев, таких как JavaScript (и, соответственно, Google Apps Script), была менее сложной, чем в некоторых компилируемых языках, которые предлагают функции, такие как детализированные иерархии исключений и всеобъемлющие инструменты отладки. Модель Google Apps Script относительно проста, используя парадигму `try-catch-finally` JavaScript. Эта простота согласуется с концепцией языка для быстрой разработки и развертывания приложений малого и среднего масштаба в экосистеме Google, но иногда может ограничивать разработчиков при работе со сложными сценариями ошибок.

В более сложных приложениях программисты часто дополняют родную обработку ошибок Google Apps Script собственными механизмами логирования и отчетности об ошибках. Это может включать запись ошибок в Google Таблицы для аудита или использование сторонних служб логирования через URL Fetch Services Google Apps Script для отправки деталей ошибок из среды скрипта.

Хотя Google Apps Script может отставать от языков, таких как Java или C#, в плане встроенной сложности и возможностей обработки ошибок, его интеграция с сервисами Google и простота подхода `try-catch-finally` делают его мощным инструментом для разработчиков, которые быстро автоматизируют задачи и создают интеграции в экосистеме Google. Разработчики из других областей могут обнаружить, что вызов заключается не столько в освоении сложных паттернов обработки ошибок, сколько в творческом использовании доступных средств для обеспечения надежности и удобства использования своих скриптов.
