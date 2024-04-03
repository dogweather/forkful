---
date: 2024-01-26 04:27:36.584402-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E8\u05D0\u05E9\
  \u05D9\u05EA, \u05EA\u05D6\u05D3\u05E7\u05E7 \u05DC\u05DE\u05E4\u05E2\u05E0\u05D7\
  \ TOML. `@iarna/toml` \u05D4\u05D5\u05D0 \u05D1\u05D7\u05D9\u05E8\u05D4 \u05E4\u05D5\
  \u05E4\u05D5\u05DC\u05E8\u05D9\u05EA. \u05D4\u05EA\u05E7\u05DF \u05D0\u05D5\u05EA\
  \u05D5 \u05E2\u05DD npm: `npm install @iarna/toml --save`. \u05D4\u05E0\u05D4 \u05D0\
  \u05D9\u05DA \u05DC\u05E7\u05E8\u05D5\u05D0 \u05E7\u05D5\u05D1\u05E5 TOML \u05D5\
  \u05DC\u05E0\u05EA\u05D7\u2026"
lastmod: '2024-03-13T22:44:38.956258-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D0\u05E9\u05D9\u05EA, \u05EA\u05D6\u05D3\u05E7\u05E7 \u05DC\u05DE\
  \u05E4\u05E2\u05E0\u05D7 TOML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

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
