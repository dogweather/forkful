---
title:                "עבודה עם TOML"
aliases:
- /he/javascript/working-with-toml.md
date:                  2024-01-26T04:23:51.878328-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML, המקוצר של Tom's Obvious, Minimal Language, מגדיר איך למבנה קבצי הגדרות. מתכנתים עובדים עם TOML מכיוון שהוא קריא, קל לכתיבה, וממופה יפה לטבלת האש, מה שהופך אותו לבחירה מועדפת להגדרות.

## איך לעשות:
כדי לעבוד עם TOML ב-JavaScript, תצטרך מפענח כמו `@iarna/toml`. קודם כל, התקן אותו: `npm install @iarna/toml`. לאחר מכן, הפוך מחרוזת TOML לאובייקט של JavaScript או הפוך אובייקט של JavaScript למחרוזת בפורמט TOML.

```javascript
const toml = require('@iarna/toml');

// פרסום מחרוזת TOML לאובייקט JS
const tomlStr = `
title = "דוגמת TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// המרת אובייקט JS למחרוזת TOML
const jsObject = {
  title: "דוגמת TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## צלילה לעומק
TOML יצא לראשונה בשנת 2013 על ידי טום פרסטון-וורנר, שותף מייסד ב-GitHub. הוא נוצר כדי להחליף פורמטים אחרים, כמו INI, על ידי היותו יותר ממוסד וקל יותר לניתוח. JSON ו-YAML הם חלופות אך יכולים להיות מורכבים מדי או גמישים מדי. יתרון TOML הוא בתצורה סטטית שבה מועדף פורמט פשוט וברור. העיצוב שלו מאפשר מיפוי ישיר לטבלת אש, עם מפתחות וערכים התואמים לשמות המאפיינים וערכיהם. לשם קליטה רחבה יותר, ייתכן שתצטרך לשלב כלים שיכולים להמיר בין TOML לפורמטים אחרים עקב תמיכה משתנה באקוסיסטמה.

## ראה גם
- המאגר הרשמי של TOML ב-GitHub: https://github.com/toml-lang/toml
- השוואה בין TOML ל-YAML ל-JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- חבילת npm `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
