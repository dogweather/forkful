---
title:                "Пошук та заміна тексту"
aliases: - /uk/google-apps-script/searching-and-replacing-text.md
date:                  2024-02-01T22:01:50.491011-07:00
model:                 gpt-4-0125-preview
simple_title:         "Пошук та заміна тексту"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Пошук та заміна тексту в Google Apps Script полягає в програмному визначенні конкретних рядків у документі, таблиці або будь-якому іншому типі вмісту Google Apps і заміні їх іншими текстовими значеннями. Програмісти використовують цю функціональність для автоматизації редагування великих обсягів вмісту, виправлення поширених помилок, уніфікації термінології у документах або вставлення динамічних даних у шаблони.

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
