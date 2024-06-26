---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:50.491011-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Google Apps Script \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043F\u0440\
  \u043E\u0441\u0442\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\u0431 \u043F\u043E\
  \u0448\u0443\u043A\u0443 \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\u0438 \u0442\
  \u0435\u043A\u0441\u0442\u0443, \u043E\u0441\u043E\u0431\u043B\u0438\u0432\u043E\
  \ \u0432 Google Docs \u0442\u0430 Sheets. \u041D\u0438\u0436\u0447\u0435 \u043D\u0430\
  \u0432\u0435\u0434\u0435\u043D\u0456 \u043F\u0440\u0438\u043A\u043B\u0430\u0434\u0438\
  \ \u0434\u043B\u044F \u043E\u0431\u043E\u0445."
lastmod: '2024-04-05T21:53:48.754712-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043F\
  \u0440\u043E\u0441\u0442\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\u0431 \u043F\
  \u043E\u0448\u0443\u043A\u0443 \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\u0438\
  \ \u0442\u0435\u043A\u0441\u0442\u0443, \u043E\u0441\u043E\u0431\u043B\u0438\u0432\
  \u043E \u0432 Google Docs \u0442\u0430 Sheets."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## Як це зробити:
Google Apps Script пропонує простий спосіб пошуку та заміни тексту, особливо в Google Docs та Sheets. Нижче наведені приклади для обох.

### Google Docs:
Для пошуку та заміни тексту в Google Документі ви переважно взаємодієте з класом `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Для пошуку та заміни конкретної фрази
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Використання
searchReplaceInDoc();
```

Цей фрагмент коду шукає всі випадки `'searchText'` у активному Google Документі та замінює їх на `'replacementText'`.

### Google Sheets:
Аналогічно, у Google Sheets ви можете використовувати `SpreadsheetApp` для виконання операцій пошуку та заміни:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Пошук та заміна в поточно активній таблиці
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Використання
searchReplaceInSheet();
```

У цьому прикладі `createTextFinder('searchText')` шукає у активному аркуші 'searchText', а `replaceAllWith('replacementText')` замінює всі випадки на 'replacementText'.

## Поглиблений огляд
Функціональність пошуку та заміни в Google Apps Script значною мірою зумовлена її веб-орієнтованою природою, що дозволяє скриптам безперешкодно маніпулювати текстом у різних програмах Google Apps. Історично ця можливість випливає з ширшого контексту обробки та маніпуляції текстом у програмуванні, де регулярні вирази та функції строк у таких мовах, як Perl та Python, задали високі стандарти гнучкості та потужності.

Хоча функціональність пошуку та заміни в Google Apps Script є потужною для простих замін, вона не має повних можливостей використання регулярних виразів, які знайдені в деяких інших мовах. Наприклад, хоча ви можете використовувати базові регулярні вирази у `createTextFinder` в Google Sheets, опції для складного зіставлення шаблонів та маніпуляції обмежені порівняно з Perl або Python.

Для більш складних потреб обробки тексту програмісти можуть вдатися до експортування вмісту Google Docs або Sheets у формат, що може бути оброблений зовнішньо з потужнішими мовами, або використовувати Google Apps Script для виклику зовнішніх API або служб, які пропонують більш складні можливості маніпуляції з текстом.

Незважаючи на ці обмеження, для більшості типових завдань пошуку та заміни в екосистемі Google Apps, Google Apps Script пропонує просте, ефективне та високо інтегроване рішення, призначене для потреб автоматизації та скриптінгу в наборі інструментів продуктивності Google.
