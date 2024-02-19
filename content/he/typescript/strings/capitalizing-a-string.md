---
aliases:
- /he/typescript/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:36.183827-07:00
description: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\
  \u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05D1\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05E0\u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\
  \u05D5\u05DC\u05D4 \u05D0\u05DD \u05D4\u05D5\u05D0 \u05D1\u05D0\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D4, \u05EA\u05D5\u05DA \u05E9\u05D0\u05E8\u05D9\u05EA \u05D4\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05E9\u05D0\u05E8\u05EA \u05DC\u05DC\u05D0\
  \ \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC. \u05E4\
  \u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5\u2026"
lastmod: 2024-02-18 23:08:52.552610
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA \u05DB\
  \u05D5\u05DC\u05DC\u05EA \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\u05EA\
  \u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\
  \u05DC\u05D4 \u05D0\u05DD \u05D4\u05D5\u05D0 \u05D1\u05D0\u05D5\u05EA \u05E7\u05D8\
  \u05E0\u05D4, \u05EA\u05D5\u05DA \u05E9\u05D0\u05E8\u05D9\u05EA \u05D4\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05E0\u05E9\u05D0\u05E8\u05EA \u05DC\u05DC\u05D0 \u05E9\
  \u05D9\u05E0\u05D5\u05D9 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC. \u05E4\u05E2\
  \u05D5\u05DC\u05D4 \u05D6\u05D5\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות ראשיות כוללת שינוי של התו הראשון במחרוזת נתונה לאות גדולה אם הוא באות קטנה, תוך שארית המחרוזת נשארת ללא שינוי בדרך כלל. פעולה זו משמשת בדרך כלל להבטחת הקפדה על כללי דקדוק לשמות עצם ייחודיים או לתחילת משפטים בעיבוד טקסט, מה שגורם לפלטים להופיע מקצועיים וקריאים.

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
