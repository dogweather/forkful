---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:55.178873-07:00
description: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\
  \u0435\u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\
  \u043E \u0440\u044F\u0434\u043A\u0430 \u0432 Google Apps Script \u0442\u0440\u043E\
  \u0445\u0438 \u043D\u0435\u0432\u0456\u0434\u043F\u043E\u0432\u0456\u0434\u043D\u0430\
  \ \u043D\u0430\u0437\u0432\u0430, \u0430\u0434\u0436\u0435, \u043D\u0430 \u0432\u0456\
  \u0434\u043C\u0456\u043D\u0443 \u0432\u0456\u0434 \u0442\u0440\u0430\u0434\u0438\
  \u0446\u0456\u0439\u043D\u0438\u0445 \u0456\u043D\u0442\u0435\u0440\u0444\u0435\u0439\
  \u0441\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430 \u0432 \u043C\u043E\u0432\u0430\u0445\u2026"
lastmod: '2024-03-13T22:44:48.539035-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\
  \u0435\u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\
  \u043E \u0440\u044F\u0434\u043A\u0430 \u0432 Google Apps Script \u0442\u0440\u043E\
  \u0445\u0438 \u043D\u0435\u0432\u0456\u0434\u043F\u043E\u0432\u0456\u0434\u043D\u0430\
  \ \u043D\u0430\u0437\u0432\u0430, \u0430\u0434\u0436\u0435, \u043D\u0430 \u0432\u0456\
  \u0434\u043C\u0456\u043D\u0443 \u0432\u0456\u0434 \u0442\u0440\u0430\u0434\u0438\
  \u0446\u0456\u0439\u043D\u0438\u0445 \u0456\u043D\u0442\u0435\u0440\u0444\u0435\u0439\
  \u0441\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430 \u0432 \u043C\u043E\u0432\u0430\u0445\u2026"
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що та Чому?

Читання аргументів командного рядка в Google Apps Script трохи невідповідна назва, адже, на відміну від традиційних інтерфейсів командного рядка в мовах програмування, як-от Python або Node.js, Google Apps Script не підтримує виконання командного рядка або аналіз аргументів за замовчуванням. Замість цього, програмісти часто імітують цей процес через користувацькі функції та параметри URL, коли запускають веб-додатки або автоматизовані завдання, дозволяючи динамічну взаємодію з функціоналом скрипта на основі введених користувачем даних або визначених параметрів.

## Як:

Щоб імітувати процес читання аргументів командного рядка в Google Apps Script, зокрема для веб-додатків, ви можете використовувати параметри рядка запиту. Коли користувач отримує доступ до URL веб-додатка, ви можете додати аргументи, наприклад, `?name=John&age=30`, та аналізувати їх у коді Apps Script. Ось як ви можете це налаштувати:

```javascript
function doGet(e) {
  var params = e.parameter; // Отримує параметри рядка запиту
  var name = params['name']; // Отримує параметр 'name'
  var age = params['age']; // Отримує параметр 'age'

  // Пробний вивід:
  var output = "Ім'я: " + name + ", Вік: " + age;
  return HtmlService.createHtmlOutput(output);
}

// Приклад URL: https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

Коли ви отримуєте доступ до URL з вказаними параметрами, скрипт виводить щось на зразок:

```
Ім'я: John, Вік: 30
```

Цей підхід є важливим для створення персоналізованих взаємодій у веб-додатках або програмного контролю виконання скриптів.

## Глибоке занурення

Аргументи командного рядка, як це розуміється в контексті традиційних мов програмування, надають можливості для скриптів та додатків обробляти параметри виконання в реальному часі, таким чином дозволяючи гнучке та динамічне виконання коду на основі введення користувача або автоматизованих процесів. Google Apps Script, будучи хмарною мовою скриптів для розробки легковагових додатків в екосистемі Google Workspace, не працює через інтерфейс командного рядка за замовчуванням. Замість цього, його виконання в значній мірі є подієво орієнтованим або вручну ініційованим через інтерфейс Apps Script і Google Workspace, або за допомогою веб-додатків, які можуть аналізувати параметри URL як псевдо аргументи командного рядка.

Враховуючи цю архітектурну відмінність, програмісти, які прийшли з бекграунду мов програмування з інтенсивним використанням CLI, можуть потребувати коригування свого підходу при автоматизації завдань або розробці додатків у Google Apps Script. Замість традиційного аналізу аргументів командного рядка, використання функціоналу веб-додатка Google Apps Script або навіть користувацьких функцій Google Sheets для інтерактивної обробки даних може слугувати аналогічними цілями. Хоча це може здатися обмеженням на перший погляд, це спонукає до розробки більш дружніх до користувача інтерфейсів та доступних веб-додатків, що відповідає акценту Google Apps Script на безперервній інтеграції та розширенні додатків Google Workspace.

Для сценаріїв, де точне імітування поведінки CLI є важливим (наприклад, при автоматизації завдань з динамічними параметрами), розробники могли б розглянути можливість використання зовнішніх платформ, які викликають веб-додатки Google Apps Script, передаючи параметри через URL як умовний "командний рядок". Однак, для нативних проєктів Google Apps Script, прийняття моделі, орієнтованої на події та інтерфейс користувача, часто призводить до більш п простих та легших для підтримки рішень.
