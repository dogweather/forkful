---
date: 2024-01-26 04:24:05.249892-07:00
description: "\u042F\u043A \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0442\
  \u0438\u0441\u044F: \u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437\
  \ TOML \u0443 JavaScript \u0432\u0430\u043C \u0431\u0443\u0434\u0435 \u043F\u043E\
  \u0442\u0440\u0456\u0431\u0435\u043D \u043F\u0430\u0440\u0441\u0435\u0440, \u043D\
  \u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, `@iarna/toml`. \u0421\u043F\u043E\
  \u0447\u0430\u0442\u043A\u0443 \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\
  \u044C \u0439\u043E\u0433\u043E: `npm install @iarna/toml`.\u2026"
lastmod: '2024-03-13T22:44:50.033637-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 TOML \u0443\
  \ JavaScript \u0432\u0430\u043C \u0431\u0443\u0434\u0435 \u043F\u043E\u0442\u0440\
  \u0456\u0431\u0435\u043D \u043F\u0430\u0440\u0441\u0435\u0440, \u043D\u0430\u043F\
  \u0440\u0438\u043A\u043B\u0430\u0434, `@iarna/toml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

## Як користуватися:
Для роботи з TOML у JavaScript вам буде потрібен парсер, наприклад, `@iarna/toml`. Спочатку встановіть його: `npm install @iarna/toml`. Після цього парсуйте TOML-рядок у об'єкт JavaScript або конвертуйте об'єкт JavaScript у формат TOML.

```javascript
const toml = require('@iarna/toml');

// Парсинг TOML рядка у JS об'єкт
const tomlStr = `
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Конвертація JS об'єкта у рядок TOML
const jsObject = {
  title: "TOML Example",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Поглиблене вивчення
TOML був вперше випущений у 2013 році Томом Престон-Вернером, співзасновником GitHub. Він був розроблений, щоб замінити інші формати, такі як INI, будучи стандартизованішим та легшим для аналізу. JSON та YAML є альтернативами, але можуть бути занадто складними або надто гнучкими. Перевага TOML полягає у статичній конфігурації, де бажано простий і зрозумілий формат. Його дизайн дозволяє просте відображення у хеш-таблицю, із ключами та значеннями, що відповідають назвам властивостей та їхній величинам. Для ширшого внеску може знадобитися інтеграція інструментів, які можуть конвертувати між TOML та іншими форматами через відмінності в підтримці екосистем.

## Дивіться також
- Офіційний репозиторій TOML на GitHub: https://github.com/toml-lang/toml
- Порівняння TOML проти YAML проти JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm пакет `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
