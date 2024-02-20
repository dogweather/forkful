---
date: 2024-01-26 04:27:36.584402-07:00
description: "TOML, \u05D4\u05DE\u05E7\u05D5\u05E6\u05E8 \u05E9\u05DC Tom's Obvious,\
  \ Minimal Language, \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E7\u05D9\
  \u05D3\u05D5\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D3\u05D5\u05DE\u05D4\
  \ \u05DC-JSON \u05D0\u05D5 YAML. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D5 \u05D1\u05E9\u05DC \u05E7\u05E8\u05D9\
  \u05D0\u05D5\u05EA\u05D5 \u05D4\u05D0\u05E0\u05D5\u05E9\u05D9\u05EA \u05D5\u05DE\
  \u05D9\u05E4\u05D5\u05D9\u05D5 \u05D4\u05D9\u05E9\u05D9\u05E8 \u05DC\u05E1\u05D5\
  \u05D2\u05D9\u2026"
lastmod: 2024-02-19 22:04:58.164799
model: gpt-4-0125-preview
summary: "TOML, \u05D4\u05DE\u05E7\u05D5\u05E6\u05E8 \u05E9\u05DC Tom's Obvious, Minimal\
  \ Language, \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E7\u05D9\u05D3\
  \u05D5\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D3\u05D5\u05DE\u05D4 \u05DC\
  -JSON \u05D0\u05D5 YAML. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D5 \u05D1\u05E9\u05DC \u05E7\u05E8\u05D9\u05D0\
  \u05D5\u05EA\u05D5 \u05D4\u05D0\u05E0\u05D5\u05E9\u05D9\u05EA \u05D5\u05DE\u05D9\
  \u05E4\u05D5\u05D9\u05D5 \u05D4\u05D9\u05E9\u05D9\u05E8 \u05DC\u05E1\u05D5\u05D2\
  \u05D9\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
---

{{< edit_this_page >}}

## מה ולמה?
TOML, המקוצר של Tom's Obvious, Minimal Language, הוא פורמט קידוד נתונים דומה ל-JSON או YAML. תכנתים משתמשים בו בשל קריאותו האנושית ומיפויו הישיר לסוגי נתונים, הופך אותו לאופציה מועדפת לקבצי תצורה והחלפת נתונים.

## איך לעשות:
ראשית, תזדקק למפענח TOML. `@iarna/toml` הוא בחירה פופולרית. התקן אותו עם npm: `npm install @iarna/toml --save`. הנה איך לקרוא קובץ TOML ולנתח אותו לאובייקט JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
אם` config.toml` מכיל:
```
[server]
port = 8080
```
הפלט יהיה:
```
{ server: { port: 8080 } }
```
וכתיבה לקובץ TOML היא פשוטה באותה המידה:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
```
הרצת הקוד הזה כותבת את האובייקט ל-`config.toml` בפורמט TOML.

## צלילה עמוקה
TOML נוצר על ידי תום פרסטון-וורנר, שותף מייסד של GitHub, בערך ב-2013 כתגובה למגבלות שהוא חש בפורמטים אחרים כמו INI או YAML. הוא עוצב להיות חד-משמעי וקל לניתוח למבני נתונים, ולכן, מועדף לקבצי תצורה. אלטרנטיבות כמו JSON חסרות תגובות, בעוד ש-YAML מורכב יותר. TOML זורח בפשטותו וביכולתו לייצג בבירור היררכיות נתונים מורכבות.

מאחורי הקלעים, כאשר אתה מנתח TOML ב-TypeScript, אתה ממיר נתונים טקסטואליים לפורמט מובנה שהשפה יכולה לתפעל. זה כולל פירוק לקסמים (המרת טקסט גולמי לאסימונים) וניתוח (בניית מבנה נתונים פנימי); `@iarna/toml` מטפל בשניהם בצורה חלקה. תמיכת האימוג'י היא מגע מהנה, המראה את הגישה הממוקדת-משתמש של TOML.

## ראה גם
- המפרט הרשמי של TOML: https://toml.io/en/
- חבילת `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- השוואות בין TOML, YAML, ו-JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
