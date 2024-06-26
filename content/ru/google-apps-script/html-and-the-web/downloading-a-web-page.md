---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:00.063173-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Google Apps Script \u0441\u043B\u0443\u0436\u0431\u0430 `UrlFetchApp`\
  \ \u0438\u043C\u0435\u0435\u0442 \u043A\u043B\u044E\u0447\u0435\u0432\u043E\u0435\
  \ \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u0435 \u0434\u043B\u044F \u0441\u043A\
  \u0430\u0447\u0438\u0432\u0430\u043D\u0438\u044F \u0432\u0435\u0431-\u043A\u043E\
  \u043D\u0442\u0435\u043D\u0442\u0430. \u041D\u0438\u0436\u0435 \u043F\u0440\u0435\
  \u0434\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u0430 \u043F\u043E\u0448\u0430\u0433\
  \u043E\u0432\u0430\u044F \u0438\u043D\u0441\u0442\u0440\u0443\u043A\u0446\u0438\u044F\
  \ \u0438\u2026"
lastmod: '2024-03-13T22:44:44.188259-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Google Apps Script \u0441\u043B\u0443\u0436\u0431\u0430 `UrlFetchApp`\
  \ \u0438\u043C\u0435\u0435\u0442 \u043A\u043B\u044E\u0447\u0435\u0432\u043E\u0435\
  \ \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u0435 \u0434\u043B\u044F \u0441\u043A\
  \u0430\u0447\u0438\u0432\u0430\u043D\u0438\u044F \u0432\u0435\u0431-\u043A\u043E\
  \u043D\u0442\u0435\u043D\u0442\u0430."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
В Google Apps Script служба `UrlFetchApp` имеет ключевое значение для скачивания веб-контента. Ниже представлена пошаговая инструкция и простой пример, демонстрирующие, как получить и зарегистрировать HTML-содержимое веб-страницы:

1. **Базовая операция получения данных:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Этот код получает HTML-содержимое example.com и регистрирует его. Это простая демонстрация получения исходного кода веб-страницы без каких-либо дополнительных параметров.

2. **Обработка перенаправлений и HTTPS:**

Для HTTPS или обработки перенаправлений код остается в основном тем же, но стоит рассмотреть реализацию обработки ошибок или специальные параметры для перенаправлений:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Автоматически следовать перенаправлениям
    'muteHttpExceptions': true // Отключить возможные исключения для их корректной обработки
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Лимиты и квоты:**

Будьте внимательны к квотам Google Apps Script; интенсивное использование может потребовать обработки ошибок из-за ограничений скорости.

## Подробнее
Исторически загрузка и манипуляция веб-контентом началась с простых HTTP-запросов, которые существенно эволюционировали с появлением языков сценариев. Google Apps Script позволяет просто выполнять такие задачи в экосистеме G Suite, используя надежную инфраструктуру Google. Служба `UrlFetchApp` является ключевым элементом этой функциональности, упаковывая сложные HTTP/S-запросы в более простой интерфейс на уровне приложения.

Несмотря на своё удобство, Google Apps Script может не всегда быть лучшим инструментом для интенсивного веб-скрепинга или когда требуется сложная последующая обработка полученных данных из-за ограничений времени выполнения и квот, наложенных Google. В таких случаях, специализированные фреймворки для веб-скрепинга или языки, предназначенные для асинхронных операций ввода-вывода, такие как Node.js с библиотеками вроде Puppeteer или Cheerio, могут предложить большую гибкость и мощность.

Более того, хотя Google Apps Script - отличный инструмент для интеграции с сервисами Google (такими как Sheets, Docs и Drive) и выполнения легковесных операций по получению данных, важно помнить о ограничениях среды исполнения. Для интенсивных задач рассмотрите использование Google Cloud Functions или продвинутых сервисов Apps Script с внешними ресурсами вычислений для обработки.
