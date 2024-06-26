---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:16.070799-07:00
description: "\u042F\u043A: Google Apps Script, \u0431\u0443\u0434\u0443\u0447\u0438\
  \ \u043C\u043E\u0432\u043E\u044E \u043D\u0430 \u043E\u0441\u043D\u043E\u0432\u0456\
  \ JavaScript, \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u0441\u0442\u0430\
  \u043D\u0434\u0430\u0440\u0442\u043D\u0456 \u043C\u0435\u0442\u043E\u0434\u0438\
  \ \u0434\u043B\u044F \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F\
  \ \u0447\u0438\u0441\u0435\u043B. \u041E\u0441\u044C \u0440\u043E\u0437\u0433\u043B\
  \u044F\u0434 \u0442\u0440\u044C\u043E\u0445 \u043F\u043E\u0448\u0438\u0440\u0435\
  \u043D\u0438\u0445 \u0442\u0435\u0445\u043D\u0456\u043A."
lastmod: '2024-04-05T21:53:48.771707-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, \u0431\u0443\u0434\u0443\u0447\u0438 \u043C\u043E\u0432\
  \u043E\u044E \u043D\u0430 \u043E\u0441\u043D\u043E\u0432\u0456 JavaScript, \u043F\
  \u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442\u043D\u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043E\
  \u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\u0435\u043B\
  ."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Як:
Google Apps Script, будучи мовою на основі JavaScript, пропонує стандартні методи для округлення чисел. Ось розгляд трьох поширених технік:

### Math.round()
Ця функція округлює число до найближчого цілого.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Виводить: 3
```

### Math.ceil()
Округлює число вгору до найближчого цілого.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Виводить: 3
```

### Math.floor()
Навпаки, округлює число вниз до найближчого цілого.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Виводить: 2
```

Для специфічної кількості десяткових знаків можна використовувати `.toFixed()`, яка насправді повертає рядок, або більш тонкий підхід для математичного округлення:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Виводить: "2.57" (як рядок)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Виводить: 2.57
```

## Поглиблений розгляд
Округлення чисел у Google Apps Script не відрізняється значно від того, як це робиться в інших середовищах JavaScript. Однак, розуміння різниці в методах округлення та потенційних проблемах арифметики з плаваючою комою є критично важливим. Наприклад, через спосіб представлення чисел з плаваючою комою комп'ютерами, не всі десяткові дроби можуть бути відтворені з абсолютною точністю, що іноді призводить до неочікуваних результатів округлення.

Історично, JavaScript (і відтак, Google Apps Script) вирішує це шляхом дотримання стандарту IEEE 754, що використовується багатьма іншими мовами програмування для арифметики з плаваючою комою. Цей стандарт визначає, як числа мають бути округлені, забезпечуючи узгодженість між різними платформами та мовами.

Хоча прямі методи округлення в Google Apps Script є простими та часто достатніми, складні або високоточні застосування можуть виграти від бібліотек на кшталт decimal.js або big.js, які призначені для виконання арифметики з довільною точністю. Це може бути особливо корисно при роботі з фінансовими або науковими обчисленнями, де точність округлених чисел є вирішальною.

Однак пам'ятайте, що використання зовнішніх бібліотек у Google Apps Script вимагає їх завантаження через редактор скриптів, що може вводити залежності або впливати на продуктивність вашого скрипту в залежності від його використання. У багатьох випадках вбудовані методи Math є абсолютно адекватними, але для тих крайніх випадків, які вимагають точності до n-го градуса, пошук альтернатив за межами стандартної бібліотеки може бути необхідним.
