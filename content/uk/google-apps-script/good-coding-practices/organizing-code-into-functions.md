---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:20.670380-07:00
description: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\
  \u043E\u0434\u0443 \u0443 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u043F\u043E\
  \u043B\u044F\u0433\u0430\u0454 \u0443 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\
  \u0440\u0443\u0432\u0430\u043D\u043D\u0456 \u0432\u0430\u0448\u043E\u0433\u043E\
  \ \u043A\u043E\u0434\u0443 Google Apps Script \u0448\u043B\u044F\u0445\u043E\u043C\
  \ \u0432\u0456\u0434\u043E\u043A\u0440\u0435\u043C\u043B\u0435\u043D\u043D\u044F\
  \ \u043B\u043E\u0433\u0456\u0447\u043D\u0438\u0445 \u0441\u0435\u0433\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u0443 \u043E\u043A\u0440\u0435\u043C\u0456 \u0431\u043B\
  \u043E\u043A\u0438, \u043A\u043E\u0436\u0435\u043D \u0437 \u044F\u043A\u0438\u0445\
  \u2026"
lastmod: '2024-03-13T22:44:48.521169-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\
  \u043E\u0434\u0443 \u0443 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u043F\u043E\
  \u043B\u044F\u0433\u0430\u0454 \u0443 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\
  \u0440\u0443\u0432\u0430\u043D\u043D\u0456 \u0432\u0430\u0448\u043E\u0433\u043E\
  \ \u043A\u043E\u0434\u0443 Google Apps Script \u0448\u043B\u044F\u0445\u043E\u043C\
  \ \u0432\u0456\u0434\u043E\u043A\u0440\u0435\u043C\u043B\u0435\u043D\u043D\u044F\
  \ \u043B\u043E\u0433\u0456\u0447\u043D\u0438\u0445 \u0441\u0435\u0433\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u0443 \u043E\u043A\u0440\u0435\u043C\u0456 \u0431\u043B\
  \u043E\u043A\u0438, \u043A\u043E\u0436\u0435\u043D \u0437 \u044F\u043A\u0438\u0445\
  \u2026"
title: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\u043E\
  \u0434\u0443 \u0443 \u0444\u0443\u043D\u043A\u0446\u0456\u0457"
---

{{< edit_this_page >}}

## Що та Чому?

Організація коду у функції полягає у структуруванні вашого коду Google Apps Script шляхом відокремлення логічних сегментів у окремі блоки, кожен з яких виконує певне завдання. Програмісти роблять це, щоб поліпшити читабельність, можливість підтримки та повторне використання коду, забезпечуючи тим самим, щоб складні сценарії було легше зрозуміти та відлагодити.

## Як:

У Google Apps Script, який базується на JavaScript, ви визначаєте функції за допомогою ключового слова `function`, за яким слідує унікальне ім'я функції, дужки `()` що можуть містити параметри, та фігурні дужки `{}`, які включають блок коду функції. Ось базовий приклад:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Привіт, ' + user + '!');
}

greetUser();
```

Приклад виведення:

```
Привіт, someone@example.com!
```

Тепер розгляньмо більш практичний приклад, пов'язаний з Google Sheets, де ми розділимо функціональність на дві функції: одну для налаштування аркуша та іншу для його заповнення даними.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Дані Продажу');
  sheet.appendRow(['Товар', 'Кількість', 'Ціна']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Дані Продажу');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// Ініціалізація масиву даних
var salesData = [
  ['Widgetи', 15, 2.5],
  ['Gadgetи', 8, 3.75]
];

// Запуск функцій
setupSheet();
populateSheet(salesData);
```

У цьому прикладі `setupSheet` готує аркуш, а `populateSheet` використовує масив даних продажів для заповнення аркуша. Розділення цих завдань робить код чистішим і більш адаптивним до змін.

## Глибоке Занурення

Концепція поділу коду на функції не є новою чи унікальною для Google Apps Script; це фундаментальна практика програмування, яка пропагується майже у всіх мовах програмування. Історично, функції еволюціонували з математичного концепту відображення вхідних даних у вихідні, що стало кутовим каменем структурованого програмування. Цей підхід сприяє модульності та повторному використанню коду, пропонуючи чіткі шляхи для тестування окремих частин сценарію.

Google Apps Script, будучи базованим на JavaScript, значно виграє від функцій першого класу в JavaScript, які дозволяють передавати функції як аргументи, повертати їх з інших функцій і призначати змінним. Ця особливість відкриває передові шаблони, як-от зворотні виклики та функціональне програмування, хоча ці шаблони можуть вносити складність, яка може бути непотрібною для простих автоматизаційних завдань у Google Apps Script.

Для більших проектів або складніших застосунків розробники можуть вивчати використання новіших можливостей JavaScript, таких як стрілкові функції (arrow functions), async/await для асинхронних операцій, а навіть TypeScript для статичної типізації. TypeScript, зокрема, може бути скомпільований для виконання як Google Apps Script, надаючи шлях для розробників, які шукають більш надійну перевірку типів та розширені особливості об'єктно-орієнтованого програмування.

Однак, для більшості сценаріїв програмування в середовищі Google Apps, дотримання простої, добре організованої структури функцій, як було продемонстровано, забезпечує міцну основу. Це завжди балансування між використанням передових можливостей для ефективності та підтримкою простоти для зручності обслуговування та читабельності.
