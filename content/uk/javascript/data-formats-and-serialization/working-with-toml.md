---
title:                "Робота з TOML"
aliases:
- /uk/javascript/working-with-toml/
date:                  2024-01-26T04:24:05.249892-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і чому?
TOML, що означає Tom's Obvious, Minimal Language (Очевидна мінімалістична мова Тома), визначає, як структурувати конфігураційні файли. Програмісти використовують TOML, тому що він легкий для читання та запису, а також гарно відображається у хеш-таблицю, що робить його зручним для конфігурацій.

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
