---
title:                "Робота з JSON"
aliases: - /uk/google-apps-script/working-with-json.md
date:                  2024-02-01T22:06:30.320838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

JSON, або JavaScript Object Notation, це легкий формат для зберігання та передачі даних, ідеальний для зв’язку між сервером і клієнтом та файлів конфігурації. Програмісти використовують його в Google Apps Script для безперебійного обміну даними між сервісами Google (як-от Таблиці, Документи, Диск) та зовнішніми джерелами завдяки його структурі, зрозумілій для людей, та легкості інтеграції в JavaScript-орієнтовані середовища.

## Як це зробити:

У Google Apps Script маніпулювання JSON є прямолінійним процесом, значною мірою завдяки вбудованій підтримці JavaScript для аналізу JSON та перетворення в рядок JSON. Ось деякі поширені операції:

**1. Аналіз JSON**: Припустимо, ми отримали рядок JSON від веб-сервісу; перетворення його в об'єкт JavaScript є суттєвим для маніпуляції даними.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Вивід: Sample Project
```

**2. Перетворення об'єктів JavaScript у рядок JSON**: Навпаки, перетворення об'єкта JavaScript на рядок JSON корисне, коли нам потрібно відправити дані з Apps Script до зовнішнього сервісу.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Вивід: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Робота зі складними даними**:
Для більш складних структур даних, таких як масиви об'єктів, процес залишається тим самим, демонструючи гнучкість JSON для представлення даних.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Вивід: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Поглиблений аналіз

Поширеність JSON у сучасних веб-додатках не можна недооцінювати, коріння якої лежить в його простоті та надзвичайній інтеграції з JavaScript, мовою вебу. Його дизайн, натхненний літералами об'єктів JavaScript, хоч і строгіший, сприяє його швидкому прийняттю. На початку 2000-х років JSON набув популярності як альтернатива XML для веб-додатків на основі AJAX, пропонуючи більш легковагий та менш многослівний формат обміну даними. З огляду на глибоку інтеграцію Google Apps Script з різними Google API та зовнішніми сервісами, JSON служить ключовим форматом для структурування, передачі та маніпулювання даними на цих платформах.

Хоча JSON залишається найпопулярнішим для веб-додатків, існують альтернативні формати даних, наприклад YAML для файлів конфігурації або Protobuf для більш ефективної бінарної серіалізації у високопродуктивних середовищах. Проте баланс JSON між читабельністю, простотою використання та широкою підтримкою серед мов програмування та інструментів закріплює його позиції як вибору за замовчуванням для багатьох розробників, які занурюються в Google Apps Script і за його межі.
