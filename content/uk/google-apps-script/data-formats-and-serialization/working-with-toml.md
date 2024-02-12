---
title:                "Працюємо з TOML"
date:                  2024-02-01T22:06:47.788091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Працюємо з TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

TOML, що означає Tom's Obvious, Minimal Language (Очевидна, Мінімалістична Мова Тома), — це формат файлу конфігурації, який легко читати через його зрозумілу семантику. Програмісти часто використовують його для файлів конфігурацій в додатках, оскільки він простий і зручний для сприйняття, що робить управління налаштуваннями та конфігураціями додатків безпроблемним у різних середовищах.

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
