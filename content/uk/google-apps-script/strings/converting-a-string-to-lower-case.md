---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:03.623573-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0440\u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\
  \u0435\u0433\u0456\u0441\u0442\u0440 \u0443 Google Apps Script, \u0445\u043C\u0430\
  \u0440\u043D\u0456\u0439 \u043C\u043E\u0432\u0456 \u0441\u043A\u0440\u0438\u043F\
  \u0442\u0456\u0432 \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u043C\u0430\u0442\
  \u0438\u0437\u0430\u0446\u0456\u0457 \u0437\u0430\u0434\u0430\u0447 \u0447\u0435\
  \u0440\u0435\u0437 \u043F\u0440\u043E\u0434\u0443\u043A\u0442\u0438 Google, \u0454\
  \ \u043E\u0441\u043D\u043E\u0432\u043D\u043E\u044E \u0437\u0430\u0434\u0430\u0447\
  \u0435\u044E,\u2026"
lastmod: '2024-03-13T22:44:48.487128-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0440\u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\
  \u0435\u0433\u0456\u0441\u0442\u0440 \u0443 Google Apps Script, \u0445\u043C\u0430\
  \u0440\u043D\u0456\u0439 \u043C\u043E\u0432\u0456 \u0441\u043A\u0440\u0438\u043F\
  \u0442\u0456\u0432 \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u043C\u0430\u0442\
  \u0438\u0437\u0430\u0446\u0456\u0457 \u0437\u0430\u0434\u0430\u0447 \u0447\u0435\
  \u0440\u0435\u0437 \u043F\u0440\u043E\u0434\u0443\u043A\u0442\u0438 Google, \u0454\
  \ \u043E\u0441\u043D\u043E\u0432\u043D\u043E\u044E \u0437\u0430\u0434\u0430\u0447\
  \u0435\u044E,\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0432 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення рядка у нижній регістр у Google Apps Script, хмарній мові скриптів для автоматизації задач через продукти Google, є основною задачею, спрямованою на стандартизацію текстових даних. Програмісти часто виконують цю дію, щоб забезпечити послідовність у введенні даних користувачем, обробці даних або при порівнянні рядків, оскільки це усуває проблеми чутливості до регістру.

## Як зробити:

Перетворення рядка у нижній регістр у Google Apps Script є простим завдяки наявним вбудованим методам JavaScript у середовищі скриптів. Метод `toLowerCase()` є тим, який ви найчастіше будете використовувати. Ось як ви можете його реалізувати:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // Виводить: hello, world!
}
```

Ця проста функція демонструє взяття оригінального рядка, застосування методу `toLowerCase()`, і виведення результату до логу. Це особливо корисно, коли ви маєте справу з введенням, яке потребує ігнорування відмінностей у регістрах. Наприклад, при порівнянні електронних адрес, які користувачі можуть вводити у різних регістрах.

Додатково, у ситуаціях, коли ви працюєте з даними у вигляді масиву, ви можете пройтися по кожному елементу, перетворюючи їх у нижній регістр:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // Виводить: [alice, bob, charlie]
}
```

Цей приклад підкреслює гнучкість `toLowerCase()` при роботі з декількома рядками даних, забезпечуючи однорідність по всьому вашому набору даних.

## Поглиблений огляд

Метод `toLowerCase()`, успадкований від JavaScript і використовуваний у Google Apps Script, був невід'ємною частиною маніпуляцій з рядками з ранніх версій JavaScript. Його основна мета - допомогти у обробці текстових даних без врахування регістру, потреба в якій з'явилася з появою динамічних, інтерактивних веб-додатків. Незважаючи на свою простоту, механізм відіграє важливу роль у валідації даних, сортуванні та алгоритмах пошуку, зменшуючи складність, внесену чутливістю до регістру.

З точки зору продуктивності, процес конвертації високо оптимізований у сучасних двигунах JavaScript; тим не менш, його застосування все ж слід використовувати обережно у великомасштабних даних операціях, щоб уникнути непотрібних витрат на обробку.

Альтернативою для розгляду, особливо при роботі зі складними шаблонами або коли потрібні конвертації, специфічні для локалі, є метод `toLocaleLowerCase()`. Ця варіація враховує специфічні правила локалі для конвертації символів у нижній регістр, що може бути важливим для додатків, які підтримують декілька мов:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // Виводить: märz
```

Незважаючи на додаткову складність, `toLocaleLowerCase()` є потужним інструментом для міжнародних додатків, забезпечуючи, що конвертація поважає лінгвістичні норми локаль користувача. Незалежно від того, який метод ви оберете, перетворення рядків у нижній регістр залишається важливою частиною обробки тексту у Google Apps Script, заповнюючи прогалину між введенням користувача і стандартизованою обробкою даних.
