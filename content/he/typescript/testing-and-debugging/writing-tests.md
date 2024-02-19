---
aliases:
- /he/typescript/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:51.653439-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1- TypeScript \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05D0\u05D5\u05D8\u05D5\u05DE\
  \u05D8\u05D9\u05D9\u05DD \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D5\u05E0\u05DB\u05D5\u05E0\
  \u05D5\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05D0\u05DE\u05D9\u05E0\u05D5\u05EA\
  , \u05DC\u05EA\u05E4\u05D5\u05E1 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D1\u05DE\u05D4\
  \u05D9\u05E8\u05D5\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.573906
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  - TypeScript \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\
  \u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\
  \u05D9\u05DD \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D5\u05E0\u05DB\u05D5\u05E0\u05D5\u05EA\
  \ \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05D4\u05D1\u05D8\u05D9\u05D7 \u05D0\u05DE\u05D9\u05E0\u05D5\u05EA, \u05DC\u05EA\
  \u05E4\u05D5\u05E1 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D1\u05DE\u05D4\u05D9\u05E8\
  \u05D5\u05EA\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות ב- TypeScript כוללת יצירת סקריפטים אוטומטיים לאימות פונקציונליות ונכונות הקוד שלך. מתכנתים עושים זאת כדי להבטיח אמינות, לתפוס באגים במהירות ולקדם גידול קוד נתון לתחזוקה, מאחר וההקלדה הסטטית של TypeScript מוסיפה רמה של ניבוי לבדיקות JavaScript.

## איך לעשות:
TypeScript פועלת בהרמוניה עם רוב מסגרות הבדיקה של JavaScript. לצורך הדגמה, נשתמש ב-Jest, מסגרת בדיקה פופולרית, בשל הטמעתה ללא תצורה לפרויקטים של TypeScript.

ראשית, ודאו ש-Jest והסוגים הדרושים של TypeScript מותקנים:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

לאחר מכן, הגדרו את Jest לעבוד עם TypeScript על ידי שינוי ה- `jest.config.js` או ביצירת קובץ חדש:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

כעת, בואו נכתוב פונקציה פשוטה ובדיקה עבורה. שקלו קובץ `sum.ts` עם הפונקציה הבאה:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

צרו קובץ בדיקה בשם `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('מוסיף 1 + 2 לקבל 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

הפעילו את הבדיקות שלכם עם:

```bash
npx jest
```

פלט לדוגמה המציין עובר בדיקה אמור להראות משהו כזה:

```plaintext
 PASS  ./sum.test.ts
  ✓ מוסיף 1 + 2 לקבל 3 (2 ms)
```

עבור קוד אסינכרוני, Jest מתאימה עם `async/await`. נניח שיש לכם פונקציה אסינכרונית `fetchData`:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

הבדיקה שלכם שמשתמשת בפונקציות אסינכרוניות:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('משיג נתונים בהצלחה', async () => {
  expect(await fetchData()).toBe('data');
});
```

כאשר אתם מריצים את הבדיקות שלכם, Jest תחכה שההבטחה תתממש, ותבצע בדיקה נכונה של פעולות אסינכרוניות.

זכרו, בדיקה אפקטיבית כוללת כתיבת מספר בדיקות לסצנריות שונות, כולל מקרי קצה, כדי לוודא שהקוד שלכם ב- TypeScript מתנהג כצפוי.
