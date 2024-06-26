---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:58.314064-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 Google Apps Script \u0454 \u043C\
  \u043E\u0432\u043E\u044E \u0441\u043A\u0440\u0438\u043F\u0442\u0456\u0432 \u0434\
  \u043B\u044F \u0440\u043E\u0437\u0440\u043E\u0431\u043A\u0438 \u043B\u0435\u0433\
  \u043A\u0438\u0445 \u0434\u043E\u0434\u0430\u0442\u043A\u0456\u0432 \u043D\u0430\
  \ \u043F\u043B\u0430\u0442\u0444\u043E\u0440\u043C\u0456 Google Apps, \u0432\u043E\
  \u043D\u0430 \u043D\u0435 \u043D\u0430\u0434\u0430\u0454 \u043F\u0440\u044F\u043C\
  \u043E\u0457 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u0444\
  \u0443\u043D\u043A\u0446\u0456\u0457\u2026"
lastmod: '2024-03-13T22:44:48.540916-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 Google Apps Script \u0454\
  \ \u043C\u043E\u0432\u043E\u044E \u0441\u043A\u0440\u0438\u043F\u0442\u0456\u0432\
  \ \u0434\u043B\u044F \u0440\u043E\u0437\u0440\u043E\u0431\u043A\u0438 \u043B\u0435\
  \u0433\u043A\u0438\u0445 \u0434\u043E\u0434\u0430\u0442\u043A\u0456\u0432 \u043D\
  \u0430 \u043F\u043B\u0430\u0442\u0444\u043E\u0440\u043C\u0456 Google Apps, \u0432\
  \u043E\u043D\u0430 \u043D\u0435 \u043D\u0430\u0434\u0430\u0454 \u043F\u0440\u044F\
  \u043C\u043E\u0457 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457\
  \ \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u043D\u0430 \u0437\u0440\u0430\u0437\
  \u043E\u043A `console.error()` \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\
  \u0443 \u0443 stderr, \u044F\u043A \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0437\u043D\u0430\u0439\u0442\u0438 \u0432 Node.js \u0447\u0438 Python."
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
weight: 25
---

## Як це зробити:
Оскільки Google Apps Script є мовою скриптів для розробки легких додатків на платформі Google Apps, вона не надає прямої вбудованої функції на зразок `console.error()` для запису у stderr, як ви можете знайти в Node.js чи Python. Однак, ви можете симулювати цю поведінку, використовуючи служби логування Google Apps Script або налаштування власної обробки помилок для управління та сегрегації виводів помилок.

### Приклад: Використання `Logger` для повідомлень про помилки
```javascript
function logError() {
  try {
    // Симуляція помилки
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Спроба поділу на нуль");
  } catch (e) {
    // Запис повідомлення про помилку у Журнали
    Logger.log('Помилка: ' + e.message);
  }
}
```

Коли ви запустите `logError()`, це запише повідомлення про помилку у журнал Google Apps Script, який ви можете переглянути через `Перегляд > Журнали`. Це не зовсім stderr, але в цьому випадку воно виконує схожу функцію відокремлення журналів помилок від стандартних виводів.

### Розширене діагностичне логування
Для більш розширеного налагодження та логування помилок ви можете використати Логування Stackdriver, яке тепер відоме як Google Cloud's Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Навмисно спричинити помилку
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Помилка: ', e.toString());
  }
}
```

Це спрямує повідомлення про помилку до Логування Stackdriver, де воно буде керуватися як журнал рівня помилки. Зверніть увагу, що інтеграція Stackdriver/Google Cloud’s Operations Suite пропонує більш деталізоване та пошукове рішення для логування порівняно з `Logger`.

## Поглиблене вивчення
Відсутність присвяченого потоку `stderr` у Google Apps Script відображає її природу та походження як мови скриптів на основі хмари, де традиційні виводи у консоль або термінал (як stdout та stderr) менш актуальні. Історично Google Apps Script було розроблено для покращення функціональності Google Apps за допомогою простих скриптів, зосереджуючись на легкості використання замість повної функціональності, доступної в більш складних середовищах програмування.

Проте, еволюція Google Apps Script у напрямку розробки більш складних додатків спонукала розробників прийняти творчі підходи до обробки помилок та логування, використовуючи доступні служби, такі як Logger, та інтегруючись з Google Cloud’s Operations Suite. Ці методи, хоча й не є прямими реалізаціями stderr, пропонують міцні альтернативи для управління помилками та діагностичного логування у хмароцентричному середовищі.

Критично важливо, що, хоча ці методи виконують свою роль у екосистемі Google Apps Script, вони підкреслюють обмеження платформи порівняно з традиційними середовищами програмування. Для розробників, які потребують детальних та ієрархічних стратегій обробки помилок, інтеграція з зовнішніми службами логування або використання Google Cloud Functions, які пропонують більш традиційне оброблення stderr та stdout, може бути більш переважною.
