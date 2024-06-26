---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:36.183827-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: TypeScript, \u05DB\
  \u05EA\u05EA-\u05E7\u05D1\u05D5\u05E6\u05D4 \u05E9\u05DC JavaScript, \u05DE\u05D0\
  \u05E4\u05E9\u05E8 \u05E9\u05D9\u05D8\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA\
  \ \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA, \u05D4\
  \u05D7\u05DC \u05DE\u05D2\u05D9\u05E9\u05D5\u05EA \u05E9\u05DC JavaScript \u05D8\
  \u05D4\u05D5\u05E8\u05D5\u05EA \u05D5\u05E2\u05D3 \u05DC\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3\u2026"
lastmod: '2024-03-13T22:44:38.889639-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u05DB\u05EA\u05EA-\u05E7\u05D1\u05D5\u05E6\u05D4 \u05E9\u05DC\
  \ JavaScript, \u05DE\u05D0\u05E4\u05E9\u05E8 \u05E9\u05D9\u05D8\u05D5\u05EA \u05E9\
  \u05D5\u05E0\u05D5\u05EA \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\
  \u05D9\u05D5\u05EA, \u05D4\u05D7\u05DC \u05DE\u05D2\u05D9\u05E9\u05D5\u05EA \u05E9\
  \u05DC JavaScript \u05D8\u05D4\u05D5\u05E8\u05D5\u05EA \u05D5\u05E2\u05D3 \u05DC\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\
  \u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DC\u05DE\u05E7\u05E8\u05D9\u05DD \u05DE\
  \u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05D0\u05D5 \u05E1\u05E6\u05D9\u05E4\u05D9\
  \u05D9\u05DD \u05D9\u05D5\u05EA\u05E8."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## איך לעשות:
TypeScript, כתת-קבוצה של JavaScript, מאפשר שיטות שונות להפוך מחרוזות לאותיות ראשיות, החל מגישות של JavaScript טהורות ועד לשימוש בספריות צד שלישי למקרים מורכבים או סציפיים יותר.

**גישת JavaScript טהורה:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// פלט לדוגמה:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

שיטה זו היא ישירה ומתבססת על המתודה `charAt()` לגישה לתו הראשון של המחרוזת ו-`toUpperCase()` כדי להמיר אותו לאות גדולה. המתודה `slice(1)` לאחר מכן מאחזרת את שאר המחרוזת, תוך שהיא נשארת ללא שינוי.

**שימוש בספריית Lodash:**

לפרויקטים שכבר משתמשים בספרייה [Lodash](https://lodash.com/), ניתן להשתמש בפונקציה `_.capitalize` שלה כדי להשיג את אותו התוצאה עם פחות קוד טמפלייט.

תחילה, התקן את Lodash:

```bash
npm install lodash
```

לאחר מכן, השתמש בה בקובץ ה-TypeScript שלך:

```typescript
import * as _ from 'lodash';

// פלט לדוגמה:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

שים לב: המתודה `_.capitalize` של Lodash ממירה את שאר המחרוזת לאותיות קטנות, מה שעשוי שלא תמיד להיות מה שאתה רוצה.

**שימוש בביטוי רגולרי:**

ביטוי רגולרי יכול להציע דרך תמציתית להפוך את האות הראשונה של מחרוזת לאות גדולה, במיוחד אם יש צורך להפוך את האות הראשונה של כל מילה במחרוזת לאות גדולה.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// פלט לדוגמה:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

שיטה זו משתמשת בפונקציה `replace()` כדי לחפש כל גבול מילה שלאחריו אות או מספר (`\b\w`), ומפעילה הגדלה של כל התאמה. זה במיוחד שימושי לכותרות או כותרות משנה.
