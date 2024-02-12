---
title:                "Получение текущей даты"
date:                  2024-02-01T21:54:56.366530-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/google-apps-script/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Получение текущей даты в Google Apps Script заключается в извлечении актуальной даты и времени, что является обыденной задачей для автоматизации процессов, ведения логов и добавления временных меток в приложениях, связанных с экосистемой Google. Программисты используют это для создания динамического контента, отслеживания сроков и планирования в рамках Google Документы, Таблицы и других сервисов Google.

## Как это сделать:

Google Apps Script, который основан на JavaScript, предлагает простые методы получения текущей даты. Вы можете использовать конструктор `new Date()` для создания нового объекта даты, представляющего текущую дату и время. Вот как вы можете манипулировать этим и отображать в различных форматах.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Выводит текущую дату и время в часовом поясе скрипта
  
  // Чтобы отобразить только дату в формате ГГГГ-ММ-ДД
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Пример вывода: "2023-04-01"
  
  // Отображение в более понятном формате
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Пример вывода: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

Эти фрагменты демонстрируют, как захватить и отформатировать текущую дату и время, демонстрируя универсальность для различных программных нужд в Google Apps Script.

## Углубление

До того как JavaScript остановился на объекте `Date`, программистам приходилось вручную отслеживать время и дату через менее стандартные и более громоздкие средства. Это включало использование целых чисел временных меток и собственноручно сделанных функций даты, которые варьировались от одной программной среды к другой, ведя к несоответствиям и проблемам совместимости.

Введение объекта `new Date()` в JavaScript, и, посредством этого, в Google Apps Script, стандартизировало операции с датой и временем, делая их более интуитивно понятными и сокращая количество кода, необходимого для операций, связанных с датой. Стоит отметить, что, хотя реализация Google Apps Script удобна и достаточна для многих приложений в наборе продуктов Google, она может не охватывать все сценарии, особенно те, которые требуют сложной обработки часовых поясов или точной регистрации временных меток в динамичных условиях.

Для таких продвинутых случаев использования программисты часто обращаются к библиотекам, таким как Moment.js или date-fns в JavaScript. Хотя Google Apps Script нативно не поддерживает эти библиотеки, разработчики могут имитировать некоторые из их функциональных возможностей, используя доступные методы JavaScript Date или обращаясь к внешним библиотекам через HTML Service или сервис URL Fetch в Google Apps Script. Несмотря на эти альтернативы, простота и интеграция родных функций даты и времени в Google Apps Script остаются предпочтительными для большинства задач экосистемы Google.