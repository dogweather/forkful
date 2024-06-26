---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:13.749306-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : TypeScript, \u05E9\u05D4\u05D5\u05D0 \u05EA\u05EA-\u05E7\u05D1\u05D5\u05E6\u05D4\
  \ \u05E9\u05DC JavaScript, \u05DE\u05E1\u05EA\u05DE\u05DA \u05E2\u05DC \u05D0\u05D5\
  \u05D1\u05D9\u05D9\u05E7\u05D8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05E6\
  \u05D5\u05E8\u05DA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05EA\u05D0\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05E2\u05DD \u05D6\
  \u05D0\u05EA, \u05D4\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D1-JS/TS \u05D9\u05DB\u05D5\u05DC\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.933228-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u05E9\u05D4\u05D5\u05D0 \u05EA\u05EA-\u05E7\u05D1\u05D5\u05E6\
  \u05D4 \u05E9\u05DC JavaScript, \u05DE\u05E1\u05EA\u05DE\u05DA \u05E2\u05DC \u05D0\
  \u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\
  \u05E6\u05D5\u05E8\u05DA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות זאת:
TypeScript, שהוא תת-קבוצה של JavaScript, מסתמך על אובייקט התאריך לצורך עיבוד תאריכים ממחרוזות. עם זאת, העבודה עם תאריכים ב-JS/TS יכולה להיות מפורטת או לא מדויקת בשל החסרונות של אובייקט התאריך. להלן דוגמה בסיסית אחריה גישה באמצעות ספרייה פופולרית, `date-fns`, לפתרונות עמידים יותר.

### באמצעות אובייקט התאריך של JavaScript
```typescript
// ניתוח בסיסי באמצעות בנאי התאריך
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// פלט ל-GMT: "Fri Apr 21 2023 15:00:00 GMT+0000 (Coordinated Universal Time)"
```

שיטה זו עובדת עבור מחרוזות בפורמט ISO וכמה פורמטים אחרים של תאריכים, אך עשויה להניב תוצאות לא עקביות עבור פורמטים לא ברורים בין דפדפנים ואזורים שונים.

### באמצעות date-fns
הספרייה `date-fns` מספקת טיפול ישיר ועקבי בתאריכים. זו ספרייה מודולרית, מה שמאפשר לך לכלול רק את החלקים שאתה זקוק להם, מה שמקטין את גודל החבילה.

ראשית, התקן את `date-fns`: 

```sh
npm install date-fns
```

לאחר מכן, השתמש בה לעיבוד מחרוזת תאריך:

```typescript
import { parseISO, format } from 'date-fns';

// ניתוח מחרוזת ISO
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// עיצוב התאריך (למשל, לצורה קריאה לאנוש)
console.log(format(parsedDate, "PPPpp")); 
// פלט: "Apr 21st, 2023 at 3:00 PM" (הפלט עשוי להשתנות בהתאם לאזור)
```

`date-fns` תומך במגוון רחב של פורמטים ואזורים, מה שהופך אותו לבחירה עמידה עבור יישומים שדורשים עיבוד ועיצוב תאריכים מדויקים באזורים שונים של משתמשים.
