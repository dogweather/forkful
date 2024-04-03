---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:47.788091-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 Google Apps Script \u043F\u043E\
  \ \u0441\u0443\u0442\u0456 \u0454 JavaScript \u0456\u0437 \u0434\u043E\u0441\u0442\
  \u0443\u043F\u043E\u043C \u0434\u043E \u043D\u0430\u0431\u043E\u0440\u0443 \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C Google, \u0440\u043E\u0431\u043E\u0442\u0430\
  \ \u0437 TOML \u0431\u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\
  \u044C\u043E \u0443 Google Apps Script\u2026"
lastmod: '2024-03-13T22:44:48.552500-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 Google Apps Script \u043F\
  \u043E \u0441\u0443\u0442\u0456 \u0454 JavaScript \u0456\u0437 \u0434\u043E\u0441\
  \u0442\u0443\u043F\u043E\u043C \u0434\u043E \u043D\u0430\u0431\u043E\u0440\u0443\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C Google, \u0440\u043E\u0431\u043E\u0442\
  \u0430 \u0437 TOML \u0431\u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\
  \u044C\u043E \u0443 Google Apps Script \u0432\u0438\u043C\u0430\u0433\u0430\u0454\
  \ \u043F\u0435\u0432\u043D\u043E\u0457 \u0432\u0438\u043D\u0430\u0445\u0456\u0434\
  \u043B\u0438\u0432\u043E\u0441\u0442\u0456."
title: "\u041F\u0440\u0430\u0446\u044E\u0454\u043C\u043E \u0437 TOML"
weight: 39
---

## Як це зробити:
Оскільки Google Apps Script по суті є JavaScript із доступом до набору програм Google, робота з TOML безпосередньо у Google Apps Script вимагає певної винахідливості. Google Apps Script не підтримує аналіз TOML уроджено, але ви можете використовувати бібліотеки JavaScript або написати простий аналізатор для базових потреб.

Давайте проаналізуємо простий рядок конфігурації TOML на прикладі:

```javascript
// Рядок TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Проста функція аналізатора TOML в JSON
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // Нова секція
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // Використовуємо eval для простоти; будьте обережні в продакшн коді
      currentSection[key] = value;
    }
  });
  return result;
}

// Тест аналізатора
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Приклад виводу з `console.log` нагадуватиме об'єкт JSON, що полегшує доступ до властивостей конфігурації в рамках Google Apps Script:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Поглиблений огляд
TOML був створений Томом Престон-Вернером, одним із засновників GitHub, щоб бути більш дружнім до людини, аніж JSON для файлів конфігурацій, одночасно зберігаючи можливість беззаперечного аналізу. Він прагне бути максимально простим, ціль, яка добре вписується в етос багатьох розробницьких проектів, що прагнуть до простоти та зрозумілості в своїх кодових базах.

У контексті Google Apps Script, використання TOML може вносити деяку надмірність, враховуючи відсутність прямої підтримки та необхідність ручного аналізу або використання сторонніх бібліотек. Для менших проектів або тих, які не глибоко інтегровані в екосистему Google, альтернативи, такі як JSON або навіть прості структури пар ключ-значення у властивостях скрипту, можуть бути достатніми і легшими для реалізації. Однак, для додатків, які надають перевагу дружнім до людини файлам конфігурацій та вже зобов'язалися використовувати TOML, інтеграція аналізу TOML за допомогою користувацьких скриптів додає корисний шар гнучкості та обслуговуваності без відхилення від обраних парадигм конфігурації.
